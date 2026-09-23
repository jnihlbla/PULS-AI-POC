000100 01  W4O74401.                                                            
000200*                                 MODCOPYTEXT TILL W40744.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDANSV-IN            PIC X(6).                                    
000800     03 IDDISTR-IN           PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR-IN          PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 KDRETSTA-IN          PIC X.                                       
001300*                                 STATUS RETURER                          
001400     03 FLSUM-IN             PIC X.                                       
001500*                                 ALLMÄN FLAGGA                           
001600     03 IDANSV-UT            PIC X(6).                                    
001700     03 IDDISTR-UT           PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 IDKUNDNR-UT          PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 KDRETSTA-UT          PIC X.                                       
002200*                                 STATUS RETURER                          
002300     03 FLSUM-UT             PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 KVANT-RT             PIC Z(4)9.                                   
002600     03 KVRADER-RT           PIC Z(4)9.                                   
002700*                                 ANTAL RADER                             
002800     03 RADER                OCCURS 11 TIMES.                             
002900*                                                                         
003000        05 KDCMD-ATTR        PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 KDCMD             PIC X.                                       
003300*                                 RAD-UPPDATERINGSKOMMANDO                
003400*                                  BLANK  = INGENTING                     
003500*                                  D , B  = DELETE                        
003600*                                  R , Ä  = REPLACE                       
003700*                                  I,N,A  = INSERT                        
003800*                                  S , V  = SELECT                        
003900*                                  P , P  = PRINT                         
004000*                                  C , K  = COPY                          
004100        05 IDPERSON-RET      PIC X(3).                                    
004200*                                 PERSONKOD RETURAVD.                     
004300        05 IDRT              PIC X(3).                                    
004400*                                 RETURTERMINAL                           
004500        05 IDRTLOP           PIC Z(2)9.                                   
004600*                                 RETUR TERMINAL LÖPNUMMER                
004700        05 TIREGDAT          PIC 9(6).                                    
004800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004900        05 TISNDDAT          PIC 9(6).                                    
005000*                                 SÄNDNINGSDATUM     (ÅÅMMDD)             
005100        05 IDRT-TRANSIT      PIC X(3).                                    
005200*                                 TRANSIT RETURTERMINAL                   
005300        05 TIREGDAT-TRRT     PIC 9(6).                                    
005400*                                 REG. DATUM I TRANSIT RET.TERM.          
005500        05 TISNDDAT-TRRT     PIC 9(6).                                    
005600*                                 SÄND. DATUM FRÅN TRANSIT RT             
005700        05 IDDISTR           PIC Z(3)9.                                   
005800*                                 DISTRIKTNUMMER                          
005900        05 IDKUNDNR          PIC Z(5)9.                                   
006000*                                 KUNDNUMMER                              
006100        05 IDRAPPNR          PIC X(7).                                    
006200*                                 RAPPORT NUMMER                          
006300        05 KVRADER           PIC Z(4)9.                                   
006400*                                 ANTAL RADER                             
006500        05 KVKOLLI-AAF       PIC Z(3)9.                                   
006600*                                 ANTAL KOLLI                             
006700     03 TEMFSINF             PIC X(55).                                   
006800*                                 INFORMATIONSMEDDELANDE                  
006900*** END OF VILMAII-COPY LENGTH= 860 BYTES                                 
