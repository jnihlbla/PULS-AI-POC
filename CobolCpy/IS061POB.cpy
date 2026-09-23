000010*** EDIT ALLOWED                                                          
000020*    COPYTEXT FRÅN EPIC , NYTT GRÄNSSNITT FÖR UPPDATERING                 
000100*                         TILL PGM KJH                                    
000030*                         PERCENT OF BUSINESS (APC-INFO)                  
000200 01  IS061POB.                                                            
000300*                             RECORD TYPE POB                             
000400     03  RT                  PIC X(3).                                    
000500*                             PLANT                                       
000600     03  PLANT               PIC X(5).                                    
000700*                             PART NO                                     
000800     03  PARTNO              PIC 9(8).                                    
001400*                             POB DATE                                    
001500     03  DATE-POB            PIC X(8).                                    
000900*                             POB PERCENT                                 
001000     03  POB                 PIC 9(3).                                    
002600*                             SUPLIER SITE                                
002700     03  SUPPLIER-ID         PIC X(5).                                    
002600*                             PART VERSION = SPACE                        
002700     03  PART-VERSION        PIC X(8).                                    
005600*** END OF VILMAII-COPY LENGTH= 040 OLD LENGTH= 040                       
