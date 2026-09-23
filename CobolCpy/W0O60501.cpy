000100 01  MOD-W0O60501.                                                        
000200*                                 COPYTEXT FÖR MOD W0O60501               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 FELMEDDELANDEFÄLT                       
000700     03 MOD-IDPRODNR-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDPRODNR-UT      PIC X(7).                                    
001000*                                 PRODUKTIONSNUMMER                       
001100     03 MOD-IDPTYP-IN        PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDPTYP-UT        PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 MOD-IDKOLLI-IN       PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDKOLLI-UT       PIC X(5).                                    
001800*                                 KOLLINUMMER                             
001900     03 MOD-KDTRSTAT-IN      PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-KDTRSTAT-UT      PIC X.                                       
002200*                                 TRANSAKTIONSSTATUS                      
002300     03 MOD-KDCMDVAL-ATTR    PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDCMDVAL         PIC X(2).                                    
002600*                                 MFS BEHANDLING AV INPUTFÄLT             
002700     03 MOD-IDPRODNR-GL      PIC 9(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900     03 MOD-IDPRODNR-NY-ATTR PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-IDPRODNR-NY      PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300     03 MOD-IDPTYP-GL        PIC X(3).                                    
003400*                                 POSTTYP                                 
003500     03 MOD-IDPTYP-NY-ATTR   PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDPTYP-NY        PIC X(2).                                    
003800*                                 MFS BEHANDLING AV INPUTFÄLT             
003900     03 MOD-IDKOLLI-GL       PIC 9(5).                                    
004000*                                 KOLLINUMMER                             
004100     03 MOD-IDKOLLI-NY-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-IDKOLLI-NY       PIC X(2).                                    
004400*                                 MFS BEHANDLING AV INPUTFÄLT             
004500     03 MOD-KDTRSTAT-GL      PIC 9.                                       
004600*                                 TRANSAKTIONSSTATUS                      
004700     03 MOD-KDTRSTAT-NY-ATTR PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KDTRSTAT-NY      PIC X(2).                                    
005000*                                 MFS BEHANDLING AV INPUTFÄLT             
005100     03 MOD-LL-GL            PIC 9(4).                                    
005200*                                 LRECL I ETT VARIABELT RECORD            
005300     03 MOD-LL-NY-ATTR       PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-LL-NY            PIC X(2).                                    
005600*                                 MFS BEHANDLING AV INPUTFÄLT             
005700     03 MOD-LINES            OCCURS 5 TIMES.                              
005800        05 MOD-TEDATA-GL     PIC X(70).                                   
005900        05 MOD-TEDATA-NY-ATTR                                             
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-TEDATA-NY     PIC X(70).                                   
006300     03 MOD-TEMFSINF         PIC X(61).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END COPY W0O60501C0  LENGTH=883                                       
