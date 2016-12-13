package org.pfcoperez.cci.hard;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

public class LongestWordAsCombination1715 {

    static class TrieNode<KE, V> {

        public TrieNode() {
            children = new HashMap<>();
            value = null;
        }

        public V find(List<KE> key) {
            return findFromIterator(key.listIterator());
        }

        public void insert(List<KE> key, V newValue) {
            insertFromIterator(key.listIterator(), newValue);
        }

        public boolean isWord() {
            return value != null;
        }

        public V getValue() {
            return value;
        }

        private V findFromIterator(ListIterator<KE> it) {
            if(!it.hasNext()) return value;
            TrieNode<KE, V> nextNode = children.get(it.next());
            if(nextNode == null) return null;
            return nextNode.findFromIterator(it);
        }

        private void insertFromIterator(ListIterator<KE> it, V newValue) {
            if(it.hasNext()) {
                KE nextKeyElement = it.next();
                TrieNode<KE, V> nextNode = children.get(nextKeyElement);
                if(nextNode == null)
                    nextNode = new TrieNode<>();
                nextNode.insertFromIterator(it, newValue);
                children.put(nextKeyElement, nextNode);
            } else {
                value = newValue;
            }
        }

        private V value;
        Map<KE, TrieNode<KE, V>> children;

    }

    public static String longestComposedWord(
            TrieNode<Character, String> root,
            TrieNode<Character, String> scout,
            TrieNode<Character, String> validator) {
        if(scout.children.isEmpty() && scout.isWord() && validator.isWord())
            return scout.getValue();
        if(scout.children.isEmpty())
            return null;
        String longest = scout.value;
        if(validator.isWord()) validator = root;
        for(Map.Entry<Character, TrieNode<Character, String>> arch: scout.children.entrySet()) {
            Character nextC = arch.getKey();
            TrieNode<Character, String> next = scout.children.get(nextC);
            TrieNode<Character, String> nextValidation = validator.children.get(nextC);
            if(next == null || nextValidation == null) continue;
            String branchResult = longestComposedWord(root, next, nextValidation);
            if(branchResult != null && (longest == null || branchResult.length() > longest.length()))
                longest = branchResult;
        }
        return longest;
    }

    public static void main(String [] args) {

        String [] input = {"walker", "walkerdog", "dog", "dogwalkerwalkerdog"};

        TrieNode<Character, String> dictionary = new TrieNode<>();

        for(String word: input) {
            List<Character> wordAsList = new LinkedList<>();
            for(int i = 0; i < word.length(); i++) wordAsList.add(word.charAt(i));
            dictionary.insert(wordAsList, word);
        }

        String result = longestComposedWord(dictionary, dictionary, dictionary);

        System.out.println(result);

    }

}
