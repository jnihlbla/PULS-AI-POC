000100 01  W4O74301.                                                            
000200*                                 MODCOPYTEXT TILL W40743.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDRTLOP-IN           PIC X(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 FLVISA-IN            PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 IDRTLOP-UT           PIC X(3).                                    
001200*                                 RETUR TERMINAL LÖPNUMMER                
001300     03 FLVISA-UT            PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500     03 IDDC-SPAR            PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 IDRT-SPAR            PIC X(3).                                    
001800*                                 RETURTERMINAL                           
001900     03 IDRTLOP-SPAR         PIC Z(2)9.                                   
002000*                                 RETUR TERMINAL LÖPNUMMER                
002100     03 IDKOLLI-SPAR         PIC 9(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 DAREGDAT-SPAR        PIC 9(8).                                    
002400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002500     03 TIKLOCK-SPAR         PIC 9(8).                                    
002600*                                 KLOCKSLAG (TTMMSSTH)                    
002700     03 INPUT.                                                            
002800*                                                                         
002900        05 FLNYSNDN-ATTR     PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 FLNYSNDN          PIC X.                                       
003200*                                 ALLMÄN FLAGGA                           
003300     03 RADER                OCCURS 11 TIMES.                             
003400*                                                                         
003500        05 KDCMD-ATTR        PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 KDCMD             PIC X.                                       
003800*                                 RAD-UPPDATERINGSKOMMANDO                
003900*                                  BLANK  = INGENTING                     
004000*                                  D , B  = DELETE                        
004100*                                  R , Ä  = REPLACE                       
004200*                                  I , N  = INSERT                        
004300*                                  S , V  = SELECT                        
004400*                                  P , P  = PRINT                         
004500*                                  C , K  = COPY                          
004600        05 IDKOLLI           PIC Z(4)9.                                   
004700*                                 KOLLINUMMER                             
004800        05 FLFARLIG          PIC X.                                       
004900*                                 FARLIGT GODS-FLAGGA                     
005000     03 FLSNDDOK-ATTR        PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 FLSNDDOK             PIC X.                                       
005300*                                 ALLMÄN FLAGGA                           
005400     03 TEMFSINF             PIC X(55).                                   
005500*                                 INFORMATIONSMEDDELANDE                  
005600*** END OF VILMAII-COPY LENGTH= 241 BYTES                                 
