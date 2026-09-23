000010*** EDIT ALLOWED                                                          
000020*    COPYTEXT FRÅN EPIC , NYTT GRÄNSSNITT FÖR UPPDATERING                 
000100*                         TILL PGM W09278                                 
000030*                         BUYER CODE                                      
000200 01  IS061BUY.                                                            
000300*                             RECORD TYPE BUY                             
000400     03  RT                  PIC X(3).                                    
000500*                             PLANT                                       
000600     03  PLANT               PIC X(5).                                    
001400*                             DATE VALID FROM                             
001500     03  DATE-VALID-FR       PIC X(8).                                    
000700*                             PART NO                                     
000800     03  PARTNO              PIC 9(8).                                    
000900*                             PURCHASER ID                                
001000     03  BUYER-CODE          PIC X(4).                                    
002600*                             PART VERSION = SPACE                        
002700     03  PART-VERSION        PIC X(8).                                    
005600*** END OF VILMAII-COPY LENGTH= 036 OLD LENGTH= 035                       
