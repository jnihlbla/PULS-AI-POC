000100 01  MOD-W4O40601.                                                        
000200*                                 COPYTEXT FÖR MOD W4O40601               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-RAD              OCCURS 39 TIMES.                             
001200*                                 COPYTEXT FÖR MOD W4O40601               
001300        05 MOD-IDPRC.                                                     
001400*                                 PRODUKTIONSKANAL                        
001500           07 MOD-IDPRCBAS   PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700           07 MOD-IDPRCVAR   PIC X.                                       
001800*                                 PRC-VARIANT                             
001900        05 MOD-IDKOLLI-PRCSTA                                             
002000                             PIC Z(4)9.                                   
002100*                                 STARTKOLLINUMMER PER DC/PRC             
002200        05 MOD-KDKOLLI       PIC X(8).                                    
002300*                                 KOLLIKOD                                
002400     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-KDCMD-IN         PIC X.                                       
002700*                                 RAD-UPPDATERINGSKOMMANDO                
002800*                                  BLANK  = INGENTING                     
002900*                                  D , B  = DELETE                        
003000*                                  R , Ä  = REPLACE                       
003100*                                  I,N,A  = INSERT                        
003200*                                  S , V  = SELECT                        
003300*                                  P , P  = PRINT                         
003400*                                  C , K  = COPY                          
003500     03 MOD-IDPRC-IN-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDPRC-IN.                                                     
003800*                                 PRODUKTIONSKANAL                        
003900        05 MOD-IDPRCBAS      PIC X(3).                                    
004000*                                 PRC-BAS                                 
004100        05 MOD-IDPRCVAR      PIC X.                                       
004200*                                 PRC-VARIANT                             
004300     03 MOD-IDKOLLI-PRCSTA-IN-ATTR                                        
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDKOLLI-PRCSTA-IN                                             
004700                             PIC Z(4)9.                                   
004800*                                 STARTKOLLINUMMER PER DC/PRC             
004900     03 MOD-KDKOLLI-IN-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-KDKOLLI-IN       PIC X(8).                                    
005200*                                 KOLLIKOD                                
005300     03 MOD-TEMFSINF         PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*** END OF VILMAII-COPY LENGTH= 792 BYTES                                 
