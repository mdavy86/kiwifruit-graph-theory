# This files imports vine architecture data and sets up the required structure for 
# using the ggraph library


all_ggraph_data <- NA

for(vine_id in 1:9){
	temp_arch <- 
		read_csv(here(paste0("input/architecture/kiwimac_data_vine", vine_id, ".csv"))) %>%
		mutate(ShootUUID = if_else(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-S"), NA),
			   CaneUUID = if_else(!is.na(cane_id), paste(vine_id, cane_id, sep = "-C"), NA),
			   OriginUUID = if_else(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-O"), NA)) %>%
		rename(VineUUID = 1, ParentNodeID = 2, NodeID = 3, Comments = 15)
	
	# Isolate all the unique node IDs
	temp_sources <- temp_arch %>%
		distinct(ParentNodeID) %>%
		rename(label = ParentNodeID)
	
	temp_targets <- temp_arch %>%
		distinct(NodeID) %>%
		rename(label = NodeID)
	
	temp_nodes <- full_join(temp_sources, temp_targets, by = "label") %>%
		arrange(label) %>%
		full_join(select(temp_arch, VineUUID, NodeID, ShootUUID, OriginUUID, Comments), 
				  by = c("label" = "NodeID")) %>%
		mutate(NodeType = if_else(!is.na(ShootUUID), "Shoot", if_else(!is.na(OriginUUID), "Origin","Junction"))) 
	
	# Sets the node ID for the origin point the segment belongs to
	# this is for tracing path lengths
	temp_nodes %<>% 
		group_by(ParentOriginID) %>%
		mutate(OriginNodeID = first(label)) %>%
		ungroup()
	
	# Vine ID not need but smart to keep in
	temp_links <- temp_arch %>%
		select(VineUUID, ParentNodeID, NodeID) %>%
		rename(from = ParentNodeID, to = NodeID)
	
}