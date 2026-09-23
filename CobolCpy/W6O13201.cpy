000100 01  MOD-W6O13201.                                                        
000200*                                 MODCOPYTEXT TILL W60132.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-ADINLOMR-IN      PIC X(4).                                    
000800*                                 INLEVERANSOMRÅDE                        
000900     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
001000*                                 INLEVERANSOMRÅDE NÄSTA                  
001100     03 MOD-KDINLQ-IN        PIC X.                                       
001200     03 MOD-BEFT-FOM-IN      PIC X(2).                                    
001300*                                 FÖRPACKNINGSTYP                         
001400     03 MOD-BEFT-TOM-IN      PIC X(2).                                    
001500*                                 FÖRPACKNINGSTYP                         
001600     03 MOD-FLINLFB-IN       PIC X.                                       
001700*                                 VALD TILL FÖRBEHANDLING                 
001800     03 MOD-IDDC-IN          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-ADINLOMR-UT      PIC X(4).                                    
002100*                                 INLEVERANSOMRÅDE                        
002200     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
002300*                                 INLEVERANSOMRÅDE NÄSTA                  
002400     03 MOD-KDINLQ-UT        PIC X.                                       
002500     03 MOD-BEFT-FOM-UT      PIC X(2).                                    
002600*                                 FÖRPACKNINGSTYP                         
002700     03 MOD-BEFT-TOM-UT      PIC X(2).                                    
002800*                                 FÖRPACKNINGSTYP                         
002900     03 MOD-FLINLFB-UT       PIC X.                                       
003000*                                 VALD TILL FÖRBEHANDLING                 
003100     03 MOD-IDDC-UT          PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 MOD-IDLEVNR-ENTER    PIC X(5).                                    
003400*                                 LEVERANTÖRNUMMER                        
003500     03 MOD-IDLEVNR-NEXT     PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700     03 MOD-IDFS-ENTER       PIC X(8).                                    
003800*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003900     03 MOD-IDFS-NEXT        PIC X(8).                                    
004000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
004100     03 MOD-TIAVIDAT-ENTER   PIC 9(6).                                    
004200*                                 AVISERINGSDATUM (YYMMDD)                
004300     03 MOD-TIAVIDAT-NEXT    PIC 9(6).                                    
004400*                                 AVISERINGSDATUM (YYMMDD)                
004500     03 MOD-IDRADNR-INL-ENTER                                             
004600                             PIC 9(9).                                    
004700*                                 ARTIKELNUMMER                           
004800     03 MOD-IDRADNR-INL-NEXT PIC 9(9).                                    
004900*                                 ARTIKELNUMMER                           
005000     03 MOD-IDRADNR-ENTER    PIC 9(5).                                    
005100*                                 RADNUMMER                               
005200     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
005300*                                 RADNUMMER                               
005400     03 MOD-VAL              OCCURS 12 TIMES.                             
005500*                                 UPDATE                                  
005600        05 MOD-KDCMDVAL-RAD-ATTR                                          
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-KDCMDVAL-RAD  PIC X(3).                                    
006000*                                 GENERELL KOMMANDOKOD                    
006100     03 MOD-IDARTNR-RAD      OCCURS 12 TIMES                              
006200                             PIC Z(7)9.                                   
006300*                                 ARTIKELNUMMER                           
006400     03 MOD-IDLEVNR-RAD      OCCURS 12 TIMES                              
006500                             PIC X(5).                                    
006600*                                 LEVERANTÖRNUMMER                        
006700     03 MOD-IDOKOLLI-RAD     OCCURS 12 TIMES                              
006800                             PIC Z(8)9.                                   
006900*                                 ODETTE KOLLINUMMER                      
007000     03 MOD-FLPREPFF-RAD     OCCURS 12 TIMES                              
007100                             PIC X.                                       
007200*                                 FÄRDIGT FÖR FÖRPACKNING?                
007300     03 MOD-FLKVROS-RAD      OCCURS 12 TIMES                              
007400                             PIC X.                                       
007500*                                 RESTORDERSALDO                          
007600     03 MOD-FLINLFB-RAD      OCCURS 12 TIMES                              
007700                             PIC X.                                       
007800*                                 VALD TILL FÖRBEHANDLING                 
007900     03 MOD-FLDIVKLI-RAD     OCCURS 12 TIMES                              
008000                             PIC X.                                       
008100*                                 DIVERSEKOLLIFLAGGA                      
008200     03 MOD-IDINLVGN-RAD     OCCURS 12 TIMES                              
008300                             PIC Z(3).                                    
008400*                                 VAGNSIDENTITET                          
008500     03 MOD-BEART-RAD        OCCURS 12 TIMES                              
008600                             PIC X(18).                                   
008700     03 MOD-KEY              OCCURS 12 TIMES.                             
008800*                                 UPDATE                                  
008900        05 MOD-IDLOPNRM-RAD  PIC 9(9).                                    
009000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
009100*                                 (0VVDLLLLK)                             
009200        05 MOD-IDRADNR-RAD   PIC 9(3).                                    
009300*                                 RADNUMMER                               
009400     03 MOD-IDINLVGN-SPAR    OCCURS 51 TIMES                              
009500                             PIC 9(3).                                    
009600*                                 VAGNSIDENTITET                          
009700     03 MOD-KVINLCAR         PIC Z(4)9.                                   
009800*                                 ANTAL VAGNAR PÅ KÖ                      
009900     03 MOD-KVRADER-HIT      PIC Z(4)9.                                   
010000*                                 ANTAL VISADE RADER HITTILS              
010100     03 MOD-KVRADER-TOT      PIC Z(4)9.                                   
010200*                                 TOTALT ANTAL RADER                      
010300     03 MOD-KVRADER-PRIO     PIC Z(4)9.                                   
010400*                                 ANTAL PRIORITERADE RADER                
010500     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
010600*                                 PRINTERPLACERING                        
010700     03 MOD-IDINLVGN-IN      PIC X(3).                                    
010800*                                 VAGNSIDENTITET                          
010900     03 MOD-IDINLVGN-UT      PIC X(3).                                    
011000*                                 VAGNSIDENTITET                          
011100     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
011200*                                 LEVERANTÖRNUMMER                        
011300     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
011400*                                 LEVERANTÖRNUMMER                        
011500     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
011600*                                 ODETTE KOLLINUMMER                      
011700     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
011800*                                 ODETTE KOLLINUMMER                      
011900     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
012000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
012100*                                 (0VVDLLLLK)                             
012200     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
012300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
012400*                                 (0VVDLLLLK)                             
012500     03 MOD-KDINLPRIO-ENTER  PIC 9(2).                                    
012600*                                 PRIORITETSGRUPP                         
012700     03 MOD-KDINLPRIO-NEXT   PIC 9(2).                                    
012800*                                 PRIORITETSGRUPP                         
012900     03 MOD-TEMFSINF         PIC X(55).                                   
013000*                                 INFORMATIONSMEDDELANDE                  
013100*** END OF VILMAII-COPY LENGTH= 1198 BYTES                                
