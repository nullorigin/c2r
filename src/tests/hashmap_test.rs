#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::Id;

    #[test]
    fn test_handlermap_basic_operations() {
        let mut map: HashMap<Id, String> = HashMap::new();
        
        // Test insertion and retrieval
        let key1 = Id::get("test1");
        let key2 = Id::get("test2");
        
        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
        
        map.insert(key1.clone(), "value1".to_string());
        map.insert(key2.clone(), "value2".to_string());
        
        assert!(!map.is_empty());
        assert_eq!(map.len(), 2);
        
        assert_eq!(map.get(&key1), Some(&"value1".to_string()));
        assert_eq!(map.get(&key2), Some(&"value2".to_string()));
        
        // Test contains_key
        assert!(map.contains_key(&key1));
        assert!(map.contains_key(&key2));
        
        // Test removal
        let removed = map.remove(&key1);
        assert_eq!(removed, Some("value1".to_string()));
        assert_eq!(map.len(), 1);
        assert!(!map.contains_key(&key1));
        assert!(map.contains_key(&key2));
        
        // Test clear
        map.clear();
        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
    }
    
    #[test]
    fn test_handlermap_iteration() {
        let mut map: HashMap<Id, i32> = HashMap::new();
        
        let key1 = Id::get("iter1");
        let key2 = Id::get("iter2");
        let key3 = Id::get("iter3");
        
        map.insert(key1.clone(), 100);
        map.insert(key2.clone(), 200);
        map.insert(key3.clone(), 300);
        
        // Test iterator
        let mut count = 0;
        for (key, value) in &map {
            count += 1;
            assert!(map.get(key) == Some(value));
        }
        assert_eq!(count, 3);
        
        // Test keys iterator
        let keys: Vec<_> = map.keys().collect();
        assert_eq!(keys.len(), 3);
        
        // Test values iterator
        let values: Vec<_> = map.values().collect();
        assert_eq!(values.len(), 3);
        
        // Test into_iter
        let consumed_map = map;
        let pairs: Vec<_> = consumed_map.into_iter().collect();
        assert_eq!(pairs.len(), 3);
    }
    
    #[test]
    fn test_handlermap_capacity_and_resize() {
        let mut map: HashMap<Id, i32> = HashMap::with_capacity(32);
        
        // Insert many items to trigger resize
        for i in 0..100 {
            let key = Id::get(&format!("key_{}", i));
            map.insert(key, i);
        }
        
        assert_eq!(map.len(), 100);
        
        // Verify all items are still accessible
        for i in 0..100 {
            let key = Id::get(&format!("key_{}", i));
            assert_eq!(map.get(&key), Some(&i));
        }
    }
}
