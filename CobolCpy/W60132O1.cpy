000100 01  RESP-W60132O1.                                                       
000200*                                 RESPCOPYTEXT TILL W6013210              
000300*                                                                         
000400     03 RESP-ADINLOMR-KEY    PIC X(4).                                    
000500*                                 INLEVERANSOMRÅDE                        
000600     03 RESP-ADINLOMR-NXT-KEY                                             
000700                             PIC X(4).                                    
000800*                                 INLEVERANSOMRÅDE                        
000900     03 RESP-IDLEVNR-KOLLI-KEY                                            
001000                             PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER KOLLI                  
001200     03 RESP-IDOKOLLI-KEY    PIC X(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400     03 RESP-IDLOPNRM-KEY    PIC X(9).                                    
001500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001600*                                 (0VVDLLLLK)                             
001700     03 RESP-IDLEVNR-START   PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 RESP-IDLEVNR-NEXT    PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100     03 RESP-IDFS-START      PIC X(8).                                    
002200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002300     03 RESP-IDFS-NEXT       PIC X(8).                                    
002400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002500     03 RESP-TIAVIDAT-START  PIC 9(6).                                    
002600*                                 AVISERINGSDATUM (YYMMDD)                
002700     03 RESP-TIAVIDAT-NEXT   PIC 9(6).                                    
002800*                                 AVISERINGSDATUM (YYMMDD)                
002900     03 RESP-IDRADNR-INL-START                                            
003000                             PIC 9(9).                                    
003100*                                 ARTIKELNUMMER                           
003200     03 RESP-IDRADNR-INL-NEXT                                             
003300                             PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500     03 RESP-IDRADNR-START   PIC 9(5).                                    
003600*                                 RADNUMMER                               
003700     03 RESP-IDRADNR-NEXT    PIC 9(5).                                    
003800*                                 RADNUMMER                               
003900     03 RESP-KDINLPRIO-START PIC 9(2).                                    
004000*                                 PRIORITETSGRUPP                         
004100     03 RESP-KDINLPRIO-NEXT  PIC 9(2).                                    
004200*                                 PRIORITETSGRUPP                         
004300     03 RESP-IDINLVGN-SPAR   OCCURS 51 TIMES                              
004400                             PIC 9(3).                                    
004500*                                 VAGNSIDENTITET                          
004600     03 RESP-KVINLCAR        PIC 9(5).                                    
004700*                                 ANTAL VAGNAR PÅ KÖ                      
004800     03 RESP-KVRADER-HIT     PIC 9(5).                                    
004900*                                 ANTAL VISADE RADER HITTILS              
005000     03 RESP-KVRADER-TOT     PIC 9(5).                                    
005100*                                 TOTALT ANTAL RADER                      
005200     03 RESP-KVRADER-PRIO    PIC 9(5).                                    
005300*                                 ANTAL PRIORITERADE RADER                
005400     03 RESP-KVRADER         PIC 9(5).                                    
005500*                                 ANTAL RADER                             
005600     03 RESP-LINE            OCCURS 1000 TIMES.                           
005700        05 RESP-KDCMDVAL-LINE                                             
005800                             PIC X(3).                                    
005900*                                 GENERELL KOMMANDOKOD                    
006000        05 RESP-KDCMDVAL-ATTR                                             
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 RESP-IDARTNR-LINE PIC Z(7)9.                                   
006400*                                 ARTIKELNUMMER                           
006500        05 RESP-IDLEVNR-LINE PIC X(5).                                    
006600*                                 LEVERANTÖRNUMMER                        
006700        05 RESP-IDOKOLLI-LINE                                             
006800                             PIC Z(8)9.                                   
006900*                                 ODETTE KOLLINUMMER                      
007000        05 RESP-FLPREPFF-LINE                                             
007100                             PIC X.                                       
007200*                                 FÄRDIGT FÖR FÖRPACKNING?                
007300        05 RESP-FLKVROS-LINE PIC X.                                       
007400*                                 RESTORDERSALDO                          
007500        05 RESP-FLINLFB-LINE PIC X.                                       
007600*                                 VALD TILL FÖRBEHANDLING                 
007700        05 RESP-FLDIVKLI-LINE                                             
007800                             PIC X.                                       
007900*                                 DIVERSEKOLLIFLAGGA                      
008000        05 RESP-IDINLVGN-LINE                                             
008100                             PIC Z(3).                                    
008200*                                 VAGNSIDENTITET                          
008300        05 RESP-BEART-LINE   PIC X(25).                                   
008400*                                 ARTIKELBENÄMNING                        
008500        05 RESP-IDLOPNRM-LINE                                             
008600                             PIC 9(9).                                    
008700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
008800*                                 (0VVDLLLLK)                             
008900        05 RESP-IDRADNR-LINE PIC 9(3).                                    
009000*                                 RADNUMMER                               
009100*** END OF VILMAII-COPY LENGTH= 71279 BYTES                               
