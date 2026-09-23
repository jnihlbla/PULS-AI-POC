000100 01  MID-W4I74301.                                                        
000200*                                 MID-COPYTEXT FÖR W40743                 
000300     03 MID-IDRTLOP-IN       PIC X(3).                                    
000400*                                 RETUR TERMINAL LÖPNUMMER                
000500     03 MID-IDRTLOP-UT       PIC X(3).                                    
000600*                                 RETUR TERMINAL LÖPNUMMER                
000700     03 MID-FLVISA-IN        PIC X.                                       
000800*                                 ALLMÄN FLAGGA                           
000900     03 MID-FLVISA-UT        PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 MID-OMSTART-NYCKLAR.                                              
001200*                                 NYCKLAR FÖR OMSTART AV PGM              
001300        05 MID-IDDC-SPAR     PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500        05 MID-IDRT-SPAR     PIC X(3).                                    
001600*                                 RETURTERMINAL                           
001700        05 MID-IDRTLOP-SPAR  PIC X(3).                                    
001800*                                 RETUR TERMINAL LÖPNUMMER                
001900        05 MID-IDKOLLI-SPAR  PIC X(5).                                    
002000*                                 KOLLINUMMER                             
002100        05 MID-DAREGDAT-SPAR PIC X(8).                                    
002200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002300        05 MID-TIKLOCK-SPAR  PIC X(8).                                    
002400*                                 KLOCKSLAG (TTMMSSTH)                    
002500     03 MID-INPUT.                                                        
002600*                                 INMATNINGSFÄLT                          
002700        05 MID-FLNYSNDN      PIC X.                                       
002800*                                 ALLMÄN FLAGGA                           
002900        05 MID-INPUTLINE     OCCURS 11 TIMES.                             
003000*                                 INMATNINGSFÄLT PÅ RADEN                 
003100           07 MID-KDCMD      PIC X.                                       
003200            88 MID-KDCMD-INGENTING                                        
003300                             VALUE ' '.                                   
003400            88 MID-KDCMD-DELETE                                           
003500                             VALUE 'D'                                    
003600                             'B'.                                         
003700            88 MID-KDCMD-REPLACE                                          
003800                             VALUE 'R'                                    
003900                             'Ä'.                                         
004000            88 MID-KDCMD-INSERT                                           
004100                             VALUE 'I'                                    
004200                             'N'.                                         
004300            88 MID-KDCMD-SELECT                                           
004400                             VALUE 'S'                                    
004500                             'V'.                                         
004600            88 MID-KDCMD-PRINT                                            
004700                             VALUE 'P'                                    
004800                             'P'.                                         
004900            88 MID-KDCMD-COPY                                             
005000                             VALUE 'C'                                    
005100                             'K'.                                         
005200*                                 RAD-UPPDATERINGSKOMMANDO                
005300*                                  BLANK  = INGENTING                     
005400*                                  D , B  = DELETE                        
005500*                                  R , Ä  = REPLACE                       
005600*                                  I , N  = INSERT                        
005700*                                  S , V  = SELECT                        
005800*                                  P , P  = PRINT                         
005900*                                  C , K  = COPY                          
006000        05 MID-FLSNDDOK      PIC X.                                       
006100*                                 ALLMÄN FLAGGA                           
006200     03 MID-KEYFIELD         OCCURS 11 TIMES.                             
006300*                                 NYCKELFÄLT PÅ RADEN                     
006400        05 MID-IDKOLLI       PIC X(5).                                    
006500*                                 KOLLINUMMER                             
006600*** END OF VILMAII-COPY LENGTH= 105 BYTES                                 
