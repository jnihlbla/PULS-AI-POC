000010*** EDIT ALLOWED                                                          
000020*    COPYTEXT FRÅN EPIC , NYTT GRÄNSSNITT FÖR UPPDATERING                 
000100*                         TILL PGM KJH                                    
000030*                         STANDARD PRICE                                  
000200 01  IS061SSK.                                                            
000300*                             RECORD TYPE SSK                             
000400     03  RT                  PIC X(3).                                    
000500*                             PLANT                                       
000600     03  PLANT               PIC X(5).                                    
000700*                             PART NO                                     
000800     03  PARTNO              PIC 9(8).                                    
001100*                             STANDARD PRICE LEGAL ENTITY                 
001100*                             POS 1-7=INTEGER PART                        
001100*                             POS 8-9=DECIMAL PART                        
001300     03  STDPRICE-LEGENT     PIC 9(9).                                    
001400*                             PRICE UNIT                                  
001500     03  PRICE-UNIT          PIC X(1).                                    
001600*                             UNIT OF MESSURE                             
001700     03  UOM                 PIC 9(1).                                    
001400*                             DATE VALID FROM                             
001500     03  DATE-VALID-FR       PIC X(8).                                    
001400*                             DATE VALID TO                               
001500     03  DATE-VALID-TO       PIC X(8).                                    
001800*                             PART VERSION = SPACE                        
002700     03  PART-VERSION        PIC X(8).                                    
002800*                             UOM ISO                                     
002900     03  UOM-ISO             PIC X(3).                                    
005600*** END OF VILMAII-COPY LENGTH= 054 OLD LENGTH= 053                       
