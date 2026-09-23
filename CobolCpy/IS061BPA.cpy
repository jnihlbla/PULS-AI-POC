000010*** EDIT ALLOWED                                                          
000020*    COPYTEXT FRÅN EPIC , NYTT GRÄNSSNITT FÖR UPPDATERING                 
000100*                         TILL PGM W09278                                 
000030*                         ORDER INFORMATION                               
000200 01  IS061BPA.                                                            
000300*                             RECORD TYPE BPA                             
000400     03  RT                  PIC X(3).                                    
000500*                             PLANT                                       
000600     03  PLANT               PIC X(5).                                    
000700*                             PART NO                                     
000800     03  PARTNO              PIC 9(8).                                    
000900*                             SUPPLIER SITE MFG                           
001000     03  SUPPLIER-ID         PIC X(5).                                    
001100*                             ORDER NUMBER                                
001300     03  ORDERNO             PIC X(12).                                   
001400*                             ORDER DATE VALID FROM                       
001500     03  ORDERDATE-FROM      PIC X(8).                                    
001600*                             ORDER TYPE                                  
001700     03  ORDTYPE             PIC X(4).                                    
001800*                             ORDER END DATE                              
002100     03  ORDERDATE-END       PIC X(8).                                    
002200*                             SUPPLIER SAL                                
002300     03  SUPPLIER-SAL        PIC X(5).                                    
002400*                             SUPPLIER SHIP FROM                          
002500     03  SUPPLIER-SHIP       PIC X(5).                                    
002600*                             MODUL PART NO                               
002700     03  PART-VERSION        PIC X(8).                                    
002800*                             EPIC CREATION DATE                          
002900     03  EPIC-CRDATE         PIC X(8).                                    
005600*** END OF VILMAII-COPY LENGTH= 079 OLD LENGTH= 079                       
