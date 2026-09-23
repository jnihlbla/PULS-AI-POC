000010 01  WXTRK6.                                                              
000020*                                 INVENTERINGSINFORMATION FRÅN            
000030*                                 DB WDH101 OCH -02.                      
000040*                                 ENDAST CDC-INFORMATION ÄR               
000050*                                 MEDTAGEN.                               
000060*                                 STOCKTAKING INFORMATION FROM            
000070*                                 DB WDH101 AND -02.                      
000080*                                 INFORMATION APPLIES ONLY ON             
000090*                                 CDC.                                    
000100     03 IDARTNR              PIC S9(9)           COMP-3.                  
000110*                                 ARTIKELNUMMER                           
000120*                                 PART NUMBER                             
000130     03 KDINVKAT             PIC S9(3)           COMP-3.                  
000140*                                 INVENTERINGSKATEGORI                    
000150*                                 STOCKTAKING CATEGORY                    
000160     03 KVJUSTKV             PIC S9(7)           COMP-3.                  
000170*                                 JUSTERAD KVANTITET                      
000180*                                 ADJUSTED QUANTITY                       
000190     03 TIM-INV              PIC S9(7)           COMP-3.                  
000200*                                 DATUM FÖR INV. ANMODAN (ÅÅMMDD)         
000210     03 TEINVANM             PIC X(25).                                   
000220*                                 INVENTERINGSANMÄRKNING                  
000230*                                 STOCKTAKING COMMENT                     
      *** END COPY WXTRK6      LENGTH=40                                        
