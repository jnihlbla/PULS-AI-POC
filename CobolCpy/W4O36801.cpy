000100 01  MOD-W4O36801.                                                        
000200*                                 MOD-COPYTEXT TILL W4036800              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPRC-IN.                                                     
000800*                                 PRODUKTIONSKANAL                        
000900        05 MOD-IDPRCBAS      PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 MOD-IDPRCVAR      PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 MOD-IDPRC-UT.                                                     
001400*                                 PRODUKTIONSKANAL                        
001500        05 MOD-IDPRCBAS      PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700        05 MOD-IDPRCVAR      PIC X.                                       
001800*                                 PRC-VARIANT                             
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-TIDATUM-IN       PIC 9(6).                                    
002400*                                 DATUM ENLIGT KDDATFORM                  
002500     03 MOD-TIDATUM-UT       PIC 9(6).                                    
002600*                                 DATUM ENLIGT KDDATFORM                  
002700     03 MOD-TIUPDATE-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-TIUPDATE-IN      PIC 9(6).                                    
003000*                                 DATUM ENLIGT KDDATFORM                  
003100     03 MOD-TIUPDATE-UT      PIC 9(6).                                    
003200*                                 DATUM ENLIGT KDDATFORM                  
003300     03 MOD-FLTABORT-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-FLTABORT         PIC X.                                       
003600*                                 BORTTAGSFLAGGA                          
003700     03 MOD-RAD              OCCURS 3 TIMES.                              
003800        05 MOD-STAPAC-IN-ATTR                                             
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-STAPAC-IN     PIC X(5).                                    
004200*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
004300        05 MOD-STAPAC-UT     PIC Z9.9(2).                                 
004400*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
004500        05 MOD-STOPAC-IN-ATTR                                             
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-STOPAC-IN     PIC X(5).                                    
004900*                                 ARBETSDAGENS SLUT (PACKNING)            
005000        05 MOD-STOPAC-UT     PIC Z9.9(2).                                 
005100*                                 ARBETSDAGENS SLUT (PACKNING)            
005200        05 MOD-STAADM-IN-ATTR                                             
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-STAADM-IN     PIC X(5).                                    
005600*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
005700        05 MOD-STAADM-UT     PIC Z9.9(2).                                 
005800*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
005900        05 MOD-STOADM-IN-ATTR                                             
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-STOADM-IN     PIC X(5).                                    
006300*                                 ARBETSDAGENS SLUT (ORDERKONT)           
006400        05 MOD-STOADM-UT     PIC Z9.9(2).                                 
006500*                                 ARBETSDAGENS SLUT (ORDERKONT)           
006600        05 MOD-STALAST-IN-ATTR                                            
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-STALAST-IN    PIC X(5).                                    
007000*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
007100        05 MOD-STALAST-UT    PIC Z9.9(2).                                 
007200*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
007300        05 MOD-STOLAST-IN-ATTR                                            
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-STOLAST-IN    PIC X(5).                                    
007700*                                 ARBETSDAGENS SLUT (LASTNING)            
007800        05 MOD-STOLAST-UT    PIC Z9.9(2).                                 
007900*                                 ARBETSDAGENS SLUT (LASTNING)            
008000     03 MOD-TEMFSINF         PIC X(55).                                   
008100*                                 INFORMATIONSMEDDELANDE                  
008200*** END OF VILMAII-COPY LENGTH= 356 BYTES                                 
