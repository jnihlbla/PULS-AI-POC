000100 01  MID-W4I40601.                                                        
000200*                                 MID-COPYTEXT FÖR W40406                 
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-INPUT.                                                        
000600*                                 MID-COPYTEXT FÖR W40406                 
000700        05 MID-KDCMD-IN      PIC X.                                       
000800*                                 RAD-UPPDATERINGSKOMMANDO                
000900*                                  BLANK  = INGENTING                     
001000*                                  D , B  = DELETE                        
001100*                                  R , Ä  = REPLACE                       
001200*                                  I,N,A  = INSERT                        
001300*                                  S , V  = SELECT                        
001400*                                  P , P  = PRINT                         
001500*                                  C , K  = COPY                          
001600        05 MID-IDPRC-IN.                                                  
001700*                                 PRODUKTIONSKANAL                        
001800           07 MID-IDPRCBAS   PIC X(3).                                    
001900*                                 PRC-BAS                                 
002000           07 MID-IDPRCVAR   PIC X.                                       
002100*                                 PRC-VARIANT                             
002200        05 MID-IDKOLLI-PRCSTA-IN                                          
002300                             PIC 9(5).                                    
002400*                                 STARTKOLLINUMMER PER DC/PRC             
002500        05 MID-KDKOLLI-IN    PIC X(8).                                    
002600*                                 KOLLIKOD                                
002700*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
