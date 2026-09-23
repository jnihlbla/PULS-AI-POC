000100 01  MOD-W6O14701.                                                        
000200*                                 MOD TO FINAL REPORTING                  
000300*                                 WITH DEVIATION                          
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDDC-IN          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 MOD-KVTRANS-UT       PIC Z(2)9.                                   
001700*                                 ANTAL TRANSAKTIONER                     
001800*                                 NUMBER OF TRANSACTIONS                  
001900     03 MOD-KVTRANS-IN-ATTR  PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-KVTRANS-IN       PIC X(3).                                    
002200*                                 ANTAL TRANSAKTIONER                     
002300*                                 NUMBER OF TRANSACTIONS                  
002400     03 MOD-LINE             OCCURS 13 TIMES.                             
002500        05 MOD-IDLOPNRM-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-IDLOPNRM      PIC X(8).                                    
002800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002900*                                 (0VVDLLLLK)                             
003000*                                 SERIAL NO RECEIVING REPORT              
003100*                                 (0WWDLLLLC)                             
003200     03 MOD-LINE             OCCURS 13 TIMES.                             
003300        05 MOD-KVANTMOT-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KVANTMOT      PIC X(6).                                    
003600*                                 ANTAL MOTTAGET                          
003700*                                 QUANTITY RECEIVED                       
003800     03 MOD-FELMEDD          OCCURS 13 TIMES                              
003900                             PIC X(40).                                   
004000*                                 MFS FELMEDDELANDE                       
004100*                                 MFS ERROR MESSAGE                       
004200     03 MOD-LINE             OCCURS 13 TIMES.                             
004300        05 MOD-FLSVAR-ATTR   PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-FLSVAR        PIC X.                                       
004600*                                 ALLMÄN SVARSFLAGGA                      
004700*                                 GENERAL REPLY FLAG                      
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*                                 INFORMATION MESSAGE                     
