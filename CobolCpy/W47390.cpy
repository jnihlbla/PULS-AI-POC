000100 01  W47390-CTX.                                                          
000200*                                 PAST TILL EMBALLAGEPROFORMA-            
000300*                                       UTSKRIFT.                         
000400     03 IDDISTR              PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 IDPRODNR             PIC S9(7)           COMP-3.                  
000900*                                 PRODUKTIONSNUMMER                       
001000     03 IDDC-SEND            PIC X(2).                                    
001100*                                 SÄNDANDE LAGER                          
001200     03 IDKUNDRF             PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400     03 KDORDKL              PIC S9              COMP-3.                  
001500      88 KDORDKL-VOR         VALUE +0.                                    
001600      88 KDORDKL-DAG         VALUE +1.                                    
001700      88 KDORDKL-2           VALUE +2.                                    
001800      88 KDORDKL-SNABB       VALUE +2.                                    
001900      88 KDORDKL-SPECIAL     VALUE +3.                                    
002000      88 KDORDKL-KVANT       VALUE +4.                                    
002100      88 KDORDKL-SATS        VALUE +5.                                    
002200*                                 ORDERKLASS                              
002300     03 W47390-001-GRP       OCCURS 24 TIMES.                             
002400*                                 EMBALLAGEÅTGÅNGS-TABELL                 
002500        05 KVPALL            PIC S9(7)           COMP-3.                  
002600*                                 ANTAL I PALL                            
002700        05 KVRAM             PIC S9(3)           COMP-3.                  
002800*                                 ANTAL RAMAR                             
002900        05 KVLOCK            PIC S9(5)           COMP-3.                  
003000*                                 ANTAL  LOCK                             
003100        05 KVEMBSPA-02       PIC S9(3)           COMP-3.                  
003200*                                 ANTAL SPACE-EMBALLAGE                   
003300     03 IDFAKT               PIC S9(7)           COMP-3.                  
003400*                                 FAKTURANUMMER                           
003500     03 KDFAKTYP             PIC X.                                       
003600      88 KDFAKTYP-HANDELS    VALUE 'R'.                                   
003700      88 KDFAKTYP-KONSIGN    VALUE 'K'.                                   
003800      88 KDFAKTYP-INTERN     VALUE 'N'.                                   
003900      88 KDFAKTYP-GRATIS     VALUE 'G'.                                   
004000      88 KDFAKTYP-TULL       VALUE 'F'.                                   
004100      88 KDFAKTYP-PROFORMA   VALUE 'P'.                                   
004200*                                 FAKTURATYP                              
004300     03 IDDC-REC             PIC X(2).                                    
004400*                                 MOTTAGANDE LAGER                        
004500*** END OF VILMAII-COPY LENGTH= 295 BYTES                                 
