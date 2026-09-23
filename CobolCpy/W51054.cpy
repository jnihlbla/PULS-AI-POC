000100 01  W51054.                                                              
000200*                                 SALDOINFORMATIONSPOST MED               
000300*                                 TIDSSTÄMPEL NÄR NEDLÄS-                 
000400*                                 NINGEN SKEDDE                           
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 DAREGDAT             PIC 9(8).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001300*                                 REGISTRATION DATE (YYYYMMDD)            
001400     03 TIKLOCK              PIC S9(9)           COMP-3.                  
001500*                                 KLOCKSLAG (TTMMSSTH)                    
001600*                                 TIME OF DAY (HHMMSSTH)                  
001700     03 KVAKS                PIC S9(7)           COMP-3.                  
001800*                                 ANKOMSTSALDO                            
001900*                                 ADVICED,NOT BINNED,QTY                  
002000     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
002100*                                 DEL AV AK PÅ VÄG                        
002200*                                 PART OF AK ON ITS WAY                   
002300     03 KVEFRS               PIC S9(7)           COMP-3.                  
002400*                                 EJ FAKTURERAT ANTAL STYCK               
002500*                                 ORDERED NOT INVOICED QTY                
002600     03 KVLS                 PIC S9(7)           COMP-3.                  
002700*                                 LAGERSALDO                              
002800*                                 STOCK BALANCE                           
002900     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
003000*                                 INKÖPSPRIS                              
003100*                                 PURCHASE PRICE                          
003200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003300*                                 ARTIKELSTANDARDPRIS                     
003400*                                 STANDARD PRICE                          
003500*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
