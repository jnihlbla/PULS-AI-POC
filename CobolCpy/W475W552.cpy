000010*** EDIT ALLOWED                                                          
000100 01  W475W552.                                                            
000200*                                                                         
000300*         LASTBÄRARE, ORDER, KOLLI OCH DETALJ                             
000400*                    UPPGIFTER LEDTEXTER I 6 MÖJLIGA SPRÅK                
000500*                                                                         
000600*        1 - SVENSKA                                                      
000700*        2 - ENGELSKA                                                     
000800*        3 - FRANSKA                                                      
000900*        4 - SPANSKA                                                      
001000*        5 - TYSKA                                                        
001200*        6 - ITALIENSKA                                                   
001210*        7 - FLAMLÄNDSKA                                                  
001300*        8 - LEDIGT                                                       
001400*                                                                         
001500*                                                                         
001600*          ****  KDLBTYP  LEDTEXTER                                       
001700*                                                                         
001800  03  BELBTYP-LEDTEXTER.                                                  
001900    05 FILLER PIC X(15) VALUE 'LASTBÄRARTYP   '.                          
002000    05 FILLER PIC X(15) VALUE 'CARRIER TYPE   '.                          
002100    05 FILLER PIC X(15) VALUE 'MOYEN TRANSPORT'.                          
002200    05 FILLER PIC X(15) VALUE 'MEDIO TRANSPORT'.                          
002300    05 FILLER PIC X(15) VALUE 'LKW-TYP        '.                          
002500    05 FILLER PIC X(15) VALUE 'MEZZO DI TRASP.'.                          
002510    05 FILLER PIC X(15) VALUE 'CONT. TYPE     '.                          
002600    05 FILLER PIC X(15) VALUE 'CARRIER TYPE   '.                          
002700*                                                                         
002800  03  FILLER  REDEFINES  BELBTYP-LEDTEXTER.                               
002900    05  BELBTYP-LEDTEXT           PIC X(15)   OCCURS  8 TIMES.            
003000    SKIP2                                                                 
003100  03 IDLBBET-LEDTEXTER.                                                   
003200    05 FILLER PIC X(12) VALUE 'LB BETECKN. '.                             
003300    05 FILLER PIC X(12) VALUE 'CARRIER DESC'.                             
003400    05 FILLER PIC X(12) VALUE 'IDENT.TRANSP'.                             
003500    05 FILLER PIC X(12) VALUE 'IDENT.TRANSP'.                             
003600    05 FILLER PIC X(12) VALUE 'BENENNUNG   '.                             
003800    05 FILLER PIC X(12) VALUE 'TRASPORTAT. '.                             
003810    05 FILLER PIC X(12) VALUE 'BESCHRYVING '.                             
003900    05 FILLER PIC X(12) VALUE 'CARRIER DESC'.                             
004000*                                                                         
004100  03  FILLER  REDEFINES  IDLBBET-LEDTEXTER.                               
004200    05  IDLBBET-LEDTEXT           PIC X(12)   OCCURS  8 TIMES.            
004300     EJECT                                                                
004400  03  IDLBSIGL-LEDTEXTER.                                                 
004500    05 FILLER PIC X(09) VALUE 'LB SIGILL'.                                
004600    05 FILLER PIC X(09) VALUE 'SEAL NR  '.                                
004700    05 FILLER PIC X(09) VALUE 'NO SCELLE'.                                
004800    05 FILLER PIC X(09) VALUE 'NRO SELLO'.                                
004900    05 FILLER PIC X(09) VALUE 'PLOMBENR '.                                
005100    05 FILLER PIC X(09) VALUE 'N.SIGILLO'.                                
005110    05 FILLER PIC X(09) VALUE 'ZEGELNR  '.                                
005200    05 FILLER PIC X(09) VALUE 'SEAL NR  '.                                
005300*                                                                         
005400  03  FILLER  REDEFINES  IDLBSIGL-LEDTEXTER.                              
005500    05  IDLBSIGL-LEDTEXT          PIC X(09)   OCCURS  8 TIMES.            
005600     SKIP2                                                                
005700*     ORDER UPGIFTER LEDTEXTER I 8 MÖJLIGA SPRÅK                          
005800     SKIP2                                                                
005900*                                                                         
006000*            ***  VOLVO ORDERNR LEDTEXTER                                 
006100*                                                                         
006200  03  VOLVO-IDPRODNR-LEDTEXTER.                                           
006300    05 FILLER PIC X(12) VALUE 'PROD.NUMMER '.                             
006400    05 FILLER PIC X(12) VALUE 'PROD.NUMBER '.                             
006500    05 FILLER PIC X(12) VALUE 'NO. DE PROD '.                             
006600    05 FILLER PIC X(12) VALUE 'NUMERO PROD '.                             
006700    05 FILLER PIC X(12) VALUE 'PROD.NUMMER '.                             
006900    05 FILLER PIC X(12) VALUE 'N. PRODUZIO.'.                             
006910    05 FILLER PIC X(12) VALUE 'PROD.NUMMER '.                             
007000    05 FILLER PIC X(12) VALUE 'PROD.NUMBER '.                             
007100*                                                                         
007200  03  FILLER  REDEFINES   VOLVO-IDPRODNR-LEDTEXTER.                       
007300    05   VOLVO-IDPRODNR-LEDTEXT      PIC X(12)   OCCURS  8 TIMES.         
007400     EJECT                                                                
007500*            ***  KUNDREFERENS  LEDTEXTER                                 
007600*                                                                         
007700  03  IDKUNDRF-LEDTEXTER.                                                 
007800    05 FILLER PIC X(10) VALUE 'ORDERNR   '.                               
007900    05 FILLER PIC X(10) VALUE 'ORDER NO  '.                               
008000    05 FILLER PIC X(10) VALUE 'ORDRE NO  '.                               
008100    05 FILLER PIC X(10) VALUE 'ORDEN NRO '.                               
008200    05 FILLER PIC X(10) VALUE 'ORDER NR  '.                               
008400    05 FILLER PIC X(10) VALUE 'ORDINE N. '.                               
008410    05 FILLER PIC X(10) VALUE 'ORDERNR   '.                               
008500    05 FILLER PIC X(10) VALUE 'ORDER NO  '.                               
008600*                                                                         
008700  03  FILLER  REDEFINES   IDKUNDRF-LEDTEXTER.                             
008800    05   IDKUNDRF-LEDTEXT           PIC X(10)   OCCURS  8 TIMES.          
008900     SKIP2                                                                
009000*            ***  KUNDREFERENS  LEDTEXTER KORT VARIANT                    
009100*                                                                         
009200  03  IDKUNDRF-LEDTEXTER-1-5.                                             
009300    05 FILLER PIC X(6) VALUE ' ORDER'.                                    
009400    05 FILLER PIC X(6) VALUE ' ORDER'.                                    
009500    05 FILLER PIC X(6) VALUE ' ORDRE'.                                    
009600    05 FILLER PIC X(6) VALUE ' ORDEN'.                                    
009700    05 FILLER PIC X(6) VALUE ' ORDER'.                                    
009800    05 FILLER PIC X(6) VALUE 'ORDINE'.                                    
009900    05 FILLER PIC X(6) VALUE ' ORDER'.                                    
010000    05 FILLER PIC X(6) VALUE ' ORDER'.                                    
010100*                                                                         
010200  03  FILLER  REDEFINES   IDKUNDRF-LEDTEXTER-1-5.                         
010300    05   IDKUNDRF-LEDT-1-5          PIC X(6)   OCCURS  8 TIMES.           
010400     EJECT                                                                
010500*            ***  VOLVO REFERENS   LEDTEXTER                              
010600*                                                                         
010700  03  BEVOLREF-LEDTEXTER.                                                 
010800    05 FILLER PIC X(12) VALUE 'KUNDENS REF.'.                             
010900    05 FILLER PIC X(12) VALUE 'CUSTOMER REF'.                             
011000    05 FILLER PIC X(12) VALUE 'REF. CLIENT '.                             
011100    05 FILLER PIC X(12) VALUE 'SU REFER.   '.                             
011200    05 FILLER PIC X(12) VALUE 'KUNDENREF   '.                             
011400    05 FILLER PIC X(12) VALUE 'RIF. CLIENTE'.                             
011410    05 FILLER PIC X(12) VALUE 'KLANTENREF  '.                             
011500    05 FILLER PIC X(12) VALUE 'CUSTOMER REF'.                             
011600*                                                                         
011700  03  FILLER  REDEFINES   BEVOLREF-LEDTEXTER.                             
011800    05   BEVOLREF-LEDTEXT           PIC X(12)   OCCURS  8 TIMES.          
011900     SKIP3                                                                
012000*            ***  KUND DATUM   LEDTEXTER                                  
012100*                                                                         
012200  03  TIREF1-LEDTEXTER.                                                   
012300    05 FILLER PIC X(12) VALUE 'KUND DATUM  '.                             
012400    05 FILLER PIC X(12) VALUE 'ORDER DATE  '.                             
012500    05 FILLER PIC X(12) VALUE 'DATE COMM.  '.                             
012600    05 FILLER PIC X(12) VALUE 'FECHA       '.                             
012700    05 FILLER PIC X(12) VALUE 'ORDER DATE  '.                             
012900    05 FILLER PIC X(12) VALUE 'DATA ORDINE '.                             
012910    05 FILLER PIC X(12) VALUE 'ORDERDATUM  '.                             
013000    05 FILLER PIC X(12) VALUE 'ORDER DATE  '.                             
013100*                                                                         
013200  03  FILLER  REDEFINES   TIREF1-LEDTEXTER.                               
013300    05   TIREF1-LEDTEXT           PIC X(12)   OCCURS  8 TIMES.            
013400     EJECT                                                                
013500*            ***  DEALER NR    LEDTEXTER                                  
013600*                                                                         
013700  03  IDDEALER-LEDTEXTER.                                                 
013800    05 FILLER PIC X(12) VALUE 'DEALER NR   '.                             
013900    05 FILLER PIC X(12) VALUE 'DEALER NO   '.                             
014000    05 FILLER PIC X(12) VALUE 'CONCESS.    '.                             
014100    05 FILLER PIC X(12) VALUE 'DEALER NR   '.                             
014200    05 FILLER PIC X(12) VALUE 'DEALER NR   '.                             
014400    05 FILLER PIC X(12) VALUE 'COD.CONC.   '.                             
014410    05 FILLER PIC X(12) VALUE 'DEALER NR   '.                             
014500    05 FILLER PIC X(12) VALUE 'DEALER NO   '.                             
014600*                                                                         
014700  03  FILLER  REDEFINES   IDDEALER-LEDTEXTER.                             
014800    05   IDDEALER-LEDTEXT           PIC X(12)   OCCURS  8 TIMES.          
014900     EJECT                                                                
015000*            ***  BEVARREF     LEDTEXTER                                  
015100*                                                                         
015200  03  BEVARREF-LEDTEXTER.                                                 
015300    05 FILLER PIC X(13) VALUE 'IMPORTÖRS REF'.                            
015400    05 FILLER PIC X(13) VALUE 'IMPORTER REF '.                            
015500    05 FILLER PIC X(13) VALUE 'REF. IMPORT  '.                            
015600    05 FILLER PIC X(13) VALUE 'REF. IMPORT  '.                            
015700    05 FILLER PIC X(13) VALUE 'IMP. REF.    '.                            
015900    05 FILLER PIC X(13) VALUE 'RIF. IMPORT. '.                            
015910    05 FILLER PIC X(13) VALUE 'REF. IMPORT  '.                            
016000    05 FILLER PIC X(13) VALUE 'IMPORTER REF '.                            
016100*                                                                         
016200  03  FILLER  REDEFINES   BEVARREF-LEDTEXTER.                             
016300    05   BEVARREF-LEDTEXT           PIC X(13)   OCCURS  8 TIMES.          
016400     EJECT                                                                
016500*        KOLLI UPGIFTER LEDTEXTER I 8 MÖJLIGA SPRÅK                       
016600*                                                                         
016700*                                                                         
016800     SKIP2                                                                
016900*            ***  IDPRODNR LEDTEXTER                                      
017000*                                                                         
017100*                                                                         
017200   03 IDPRODNR-LEDTEXTER.                                                 
017300     05 FILLER PIC X(06) VALUE 'PRODNR'.                                  
017400     05 FILLER PIC X(06) VALUE 'PRODNR'.                                  
017500     05 FILLER PIC X(06) VALUE 'PRODNR'.                                  
017600     05 FILLER PIC X(06) VALUE 'PRODNR'.                                  
017700     05 FILLER PIC X(06) VALUE 'PRODNR'.                                  
017800     05 FILLER PIC X(06) VALUE 'PRODNR'.                                  
017900     05 FILLER PIC X(06) VALUE 'PRODNR'.                                  
018000     05 FILLER PIC X(06) VALUE 'PRODNR'.                                  
018100*                                                                         
018200   03  FILLER  REDEFINES  IDPRODNR-LEDTEXTER.                             
018300     05  IDPRODNR-LEDTEXT           PIC X(06)  OCCURS  8 TIMES.           
018400     SKIP2                                                                
018500   03 IDKOLLI-LEDTEXTER.                                                  
018600     05 FILLER PIC X(05) VALUE 'KOLLI'.                                   
018700     05 FILLER PIC X(05) VALUE 'CASE '.                                   
018800     05 FILLER PIC X(05) VALUE 'COLIS'.                                   
018900     05 FILLER PIC X(05) VALUE 'CAJA '.                                   
019000     05 FILLER PIC X(05) VALUE 'KISTE'.                                   
019200     05 FILLER PIC X(05) VALUE 'CASSA'.                                   
019210     05 FILLER PIC X(05) VALUE 'KIST '.                                   
019300     05 FILLER PIC X(05) VALUE 'CASE '.                                   
019400*                                                                         
019500   03  FILLER  REDEFINES  IDKOLLI-LEDTEXTER.                              
019600     05  IDKOLLI-LEDTEXT           PIC X(05)  OCCURS  8 TIMES.            
019700     SKIP2                                                                
019800   03 IDKUNDRF-IDKOLLI-LEDTEXTER.                                         
019900     05 FILLER PIC X(11) VALUE ' KOLLI-NR'.                               
020000     05 FILLER PIC X(11) VALUE ' PACKAGE-NO'.                             
020100     05 FILLER PIC X(11) VALUE ' COLIS-NO'.                               
020200     05 FILLER PIC X(11) VALUE ' BULTO-NO'.                               
020300     05 FILLER PIC X(11) VALUE ' KOLLI-NR'.                               
020500     05 FILLER PIC X(11) VALUE ' N. COLLI'.                               
020510     05 FILLER PIC X(11) VALUE ' KOLLI-NR'.                               
020600     05 FILLER PIC X(11) VALUE ' PACKAGE-NO'.                             
020700*                                                                         
020800   03  FILLER  REDEFINES  IDKUNDRF-IDKOLLI-LEDTEXTER.                     
020900     05  IDKUNDRF-IDKOLLI-LEDTEXT  PIC X(11)  OCCURS  8 TIMES.            
021000     SKIP2                                                                
021100   03 IDKUNDRF-IDKOLLI-TOT-LEDTEXTER.                                     
021200     05 FILLER PIC X(7) VALUE 'KOLLI  '.                                  
021300     05 FILLER PIC X(7) VALUE 'PACKAGE'.                                  
021400     05 FILLER PIC X(7) VALUE 'COLIS  '.                                  
021500     05 FILLER PIC X(7) VALUE 'BULTO  '.                                  
021600     05 FILLER PIC X(7) VALUE 'KOLLI  '.                                  
021800     05 FILLER PIC X(7) VALUE 'COLLI  '.                                  
021810     05 FILLER PIC X(7) VALUE 'KOLLI  '.                                  
021900     05 FILLER PIC X(7) VALUE 'PACKAGE'.                                  
022000*                                                                         
022100   03  FILLER  REDEFINES  IDKUNDRF-IDKOLLI-TOT-LEDTEXTER.                 
022200     05  IDKUNDRF-IDKOLLI-TOT-LEDTEXT PIC X(7)  OCCURS  8 TIMES.          
022300     SKIP2                                                                
022400*            ***  KDEMBTYP    LEDTEXTER                                   
022500*                                                                         
022600*                                                                         
022700   03 BEEMBTYP-LEDTEXTER.                                                 
022800     05 FILLER PIC X(12) VALUE 'EMBALLAGETYP'.                            
022900     05 FILLER PIC X(12) VALUE 'PACKING TYPE'.                            
023000     05 FILLER PIC X(12) VALUE 'EMBALLAGE   '.                            
023100     05 FILLER PIC X(12) VALUE 'TIPO EMBALAJ'.                            
023200     05 FILLER PIC X(12) VALUE 'VERP. TYP   '.                            
023400     05 FILLER PIC X(12) VALUE 'TIPO IMBALLO'.                            
023410     05 FILLER PIC X(12) VALUE 'VERP. TYPE  '.                            
023500     05 FILLER PIC X(12) VALUE 'PACKING TYPE'.                            
023600*                                                                         
023700   03  FILLER  REDEFINES  BEEMBTYP-LEDTEXTER.                             
023800     05  BEEMBTYP-LEDTEXT           PIC X(12)  OCCURS  8 TIMES.           
023900     SKIP2                                                                
024000*            ***  KOLLI LÄNGD LEDTEXTER                                   
024100*                                                                         
024200*                                                                         
024300   03 DIKOLLIL-LEDTEXTER.                                                 
024400     05 FILLER PIC X(05) VALUE 'LÄNGD'.                                   
024500     05 FILLER PIC X(05) VALUE 'LNGTH'.                                   
024600     05 FILLER PIC X(05) VALUE 'LONG.'.                                   
024700     05 FILLER PIC X(05) VALUE 'LONG '.                                   
024800     05 FILLER PIC X(05) VALUE '    L'.                                   
025000     05 FILLER PIC X(05) VALUE 'LUNG.'.                                   
025010     05 FILLER PIC X(05) VALUE '    L'.                                   
025100     05 FILLER PIC X(05) VALUE 'LNGTH'.                                   
025200*                                                                         
025300   03  FILLER  REDEFINES  DIKOLLIL-LEDTEXTER.                             
025400     05  DIKOLLIL-LEDTEXT           PIC X(05)  OCCURS  8 TIMES.           
025500     SKIP2                                                                
025600*            ***  KOLLI BREDD LEDTEXTER                                   
025700*                                                                         
025800*                                                                         
025900   03 DIKOLLIB-LEDTEXTER.                                                 
026000     05 FILLER PIC X(03) VALUE 'BRD'.                                     
026100     05 FILLER PIC X(03) VALUE 'WST'.                                     
026200     05 FILLER PIC X(03) VALUE 'LRG'.                                     
026300     05 FILLER PIC X(03) VALUE 'ANC'.                                     
026400     05 FILLER PIC X(03) VALUE '  B'.                                     
026600     05 FILLER PIC X(03) VALUE 'LAR'.                                     
026610     05 FILLER PIC X(03) VALUE '  B'.                                     
026700     05 FILLER PIC X(03) VALUE 'WST'.                                     
026800*                                                                         
026900   03  FILLER  REDEFINES  DIKOLLIB-LEDTEXTER.                             
027000     05  DIKOLLIB-LEDTEXT           PIC X(03)  OCCURS  8 TIMES.           
027100     SKIP2                                                                
027200*            ***  KOLLI HÖJD  LEDTEXTER                                   
027300*                                                                         
027400*                                                                         
027500   03 DIKOLLIH-LEDTEXTER.                                                 
027600     05 FILLER PIC X(03) VALUE 'HJD'.                                     
027700     05 FILLER PIC X(03) VALUE 'HGH'.                                     
027800     05 FILLER PIC X(03) VALUE 'HTR'.                                     
027900     05 FILLER PIC X(03) VALUE 'ALT'.                                     
028000     05 FILLER PIC X(03) VALUE '  H'.                                     
028200     05 FILLER PIC X(03) VALUE 'ALT'.                                     
028210     05 FILLER PIC X(03) VALUE '  H'.                                     
028300     05 FILLER PIC X(03) VALUE 'HGH'.                                     
028400*                                                                         
028500   03  FILLER  REDEFINES  DIKOLLIH-LEDTEXTER.                             
028600     05  DIKOLLIH-LEDTEXT           PIC X(03)  OCCURS  8 TIMES.           
028700     SKIP2                                                                
028800*            ***  BRUTTOVIKT      LEDTEXTER                               
028900*                                                                         
029000  03  VKORDBTO-LEDTEXTER.                                                 
029100    05 FILLER PIC X(08) VALUE 'BTO.VIKT'.                                 
029200    05 FILLER PIC X(08) VALUE 'GROSS WG'.                                 
029300    05 FILLER PIC X(08) VALUE 'POIDS B.'.                                 
029400    05 FILLER PIC X(08) VALUE 'PESO BR.'.                                 
029500    05 FILLER PIC X(08) VALUE ' BRTO GW'.                                 
029700    05 FILLER PIC X(08) VALUE ' P/LORDO'.                                 
029710    05 FILLER PIC X(08) VALUE ' BRTO GW'.                                 
029800    05 FILLER PIC X(08) VALUE 'GROSS WG'.                                 
029900*                                                                         
030000  03  FILLER  REDEFINES  VKORDBTO-LEDTEXTER.                              
030100    05  VKORDBTO-LEDTEXT           PIC X(08)   OCCURS  8 TIMES.           
030200     SKIP2                                                                
030300*            ***  NETTOVIKT      LEDTEXTER                                
030400*                                                                         
030500  03  VKORDNTO-LEDTEXTER.                                                 
030600    05 FILLER PIC X(08) VALUE 'NTO.VIKT'.                                 
030700    05 FILLER PIC X(08) VALUE 'NET WGHT'.                                 
030800    05 FILLER PIC X(08) VALUE 'POIDS NT'.                                 
030900    05 FILLER PIC X(08) VALUE 'PESO NTO'.                                 
031000    05 FILLER PIC X(08) VALUE 'NETTO GW'.                                 
031200    05 FILLER PIC X(08) VALUE 'P/NETTO '.                                 
031210    05 FILLER PIC X(08) VALUE 'NETTO GW'.                                 
031300    05 FILLER PIC X(08) VALUE 'NET WGHT'.                                 
031400*                                                                         
031500  03  FILLER  REDEFINES  VKORDNTO-LEDTEXTER.                              
031600    05  VKORDNTO-LEDTEXT           PIC X(08)   OCCURS  8 TIMES.           
031700     SKIP2                                                                
031800*            ***  BRUTTOVOLYM      LEDTEXTER                              
031900*                                                                         
032000  03  VLORDBTO-LEDTEXTER.                                                 
032100    05 FILLER PIC X(08) VALUE '   VOLYM'.                                 
032200    05 FILLER PIC X(08) VALUE '  VOLUME'.                                 
032300    05 FILLER PIC X(08) VALUE '  VOLUME'.                                 
032400    05 FILLER PIC X(08) VALUE ' VOLUMEN'.                                 
032500    05 FILLER PIC X(08) VALUE ' VOLUMEN'.                                 
032700    05 FILLER PIC X(08) VALUE '  VOLUME'.                                 
032710    05 FILLER PIC X(08) VALUE '  VOLUME'.                                 
032800    05 FILLER PIC X(08) VALUE '  VOLUME'.                                 
032900*                                                                         
033000  03  FILLER  REDEFINES  VLORDBTO-LEDTEXTER.                              
033100    05  VLORDBTO-LEDTEXT           PIC X(08)   OCCURS  8 TIMES.           
033200     SKIP2                                                                
033300*            ***  ORDERVÄRDE  LEDTEXTER                                   
033400*                                                                         
033500  03  SUORDV-LEDTEXTER.                                                   
033600    05 FILLER PIC X(08) VALUE '   VÄRDE'.                                 
033700    05 FILLER PIC X(08) VALUE '   VALUE'.                                 
033800    05 FILLER PIC X(08) VALUE '  VALEUR'.                                 
033900    05 FILLER PIC X(08) VALUE '   VALOR'.                                 
034000    05 FILLER PIC X(08) VALUE '    WERT'.                                 
034200    05 FILLER PIC X(08) VALUE '  VALORE'.                                 
034210    05 FILLER PIC X(08) VALUE '  WAARDE'.                                 
034300    05 FILLER PIC X(08) VALUE '   VALUE'.                                 
034400*                                                                         
034500  03  FILLER  REDEFINES  SUORDV-LEDTEXTER.                                
034600    05  SUORDV-LEDTEXT           PIC X(08)   OCCURS  8 TIMES.             
034700     SKIP2                                                                
034800*            ***  KDFARLIG    LEDTEXTER                                   
034900*                                                                         
035000*                                                                         
035100   03 KDFARLIG-TEXTER.                                                    
035200     05 FILLER PIC X(17) VALUE '     FARLIGT GODS'.                       
035300     05 FILLER PIC X(17) VALUE '        HAZARDOUS'.                       
035400     05 FILLER PIC X(17) VALUE 'PRODUIT DANGEREUX'.                       
035500     05 FILLER PIC X(17) VALUE '      MERC.PELIG.'.                       
035600     05 FILLER PIC X(17) VALUE 'BESCHREUKTER ART.'.                       
035800     05 FILLER PIC X(17) VALUE ' PROD. PERICOLOSO'.                       
035810     05 FILLER PIC X(17) VALUE 'GEVAARLYKE GOEDER'.                       
035900     05 FILLER PIC X(17) VALUE '        HAZARDOUS'.                       
036000*                                                                         
036100   03  FILLER  REDEFINES  KDFARLIG-TEXTER.                                
036200     05  BEFARLIG-TEXT              PIC X(17)  OCCURS  8 TIMES.           
036300     SKIP2                                                                
036400*            ***  TULLGRPNUMMER  LEDTEXTER                                
036500*                                                                         
036600*                                                                         
036700   03 KVFLAMP-LEDTEXTER.                                                  
036800     05 FILLER PIC X(06) VALUE 'FLAM.P'.                                  
036900     05 FILLER PIC X(06) VALUE 'FLAM.P'.                                  
037000     05 FILLER PIC X(06) VALUE 'P.INFL'.                                  
037100     05 FILLER PIC X(06) VALUE 'P.INFL'.                                  
037200     05 FILLER PIC X(06) VALUE 'FLAM.P'.                                  
037300     05 FILLER PIC X(06) VALUE 'INFIAM'.                                  
037400     05 FILLER PIC X(06) VALUE 'FLAM.P'.                                  
037500     05 FILLER PIC X(06) VALUE 'FLAM.P'.                                  
037600*                                                                         
037700   03  FILLER  REDEFINES  KVFLAMP-LEDTEXTER.                              
037800     05  KVFLAMP-LEDTEXT       PIC X(06)  OCCURS  8 TIMES.                
037900     SKIP2                                                                
038000*            ***  TULLGRPNUMMER  LEDTEXTER                                
038100*                                                                         
038200*                                                                         
038300   03 IDTULLG-LEDTEXTER.                                                  
038400     05 FILLER PIC X(10) VALUE 'TULLGRP NR'.                              
038500     05 FILLER PIC X(10) VALUE 'TARIFF NO '.                              
038600     05 FILLER PIC X(10) VALUE 'N.DE TARIF'.                              
038700     05 FILLER PIC X(10) VALUE 'AR. ADUANA'.                              
038800     05 FILLER PIC X(10) VALUE 'TARIF NR  '.                              
039000     05 FILLER PIC X(10) VALUE 'TARIFFA N.'.                              
039010     05 FILLER PIC X(10) VALUE 'TARIEF NR '.                              
039100     05 FILLER PIC X(10) VALUE 'TARIFF NO '.                              
039200*                                                                         
039300   03  FILLER  REDEFINES  IDTULLG-LEDTEXTER.                              
039400     05  IDTULLG-LEDTEXT       PIC X(10)  OCCURS  8 TIMES.                
039500     SKIP2                                                                
039600*            ***  TULLGRUPPS BEN  LEDTEXTER                               
039700*                                                                         
039800*                                                                         
039900   03 BETULLG-LEDTEXTER.                                                  
040000     05 FILLER PIC X(32) VALUE 'TULLGRUPPSBENÄMNING             '.        
040100     05 FILLER PIC X(32) VALUE 'TARIFF NAME                     '.        
040200     05 FILLER PIC X(32) VALUE 'DESIGNATION TARIF               '.        
040300     05 FILLER PIC X(32) VALUE 'FORMULA ARANCELARIA             '.        
040400     05 FILLER PIC X(32) VALUE 'TARIF NAME                      '.        
040600     05 FILLER PIC X(32) VALUE 'NOME TARIFFA                    '.        
040610     05 FILLER PIC X(32) VALUE 'TARIEF                          '.        
040700     05 FILLER PIC X(32) VALUE 'TARIFF NAME                     '.        
040800*                                                                         
040900   03  FILLER  REDEFINES  BETULLG-LEDTEXTER.                              
041000     05  BETULLG-LEDTEXT           PIC X(32)  OCCURS  8 TIMES.            
041100     EJECT                                                                
041200*           DETALJ RAD LEDTEXTER I 8 MÖJLIGA SPRÅK                        
041300*                                                                         
041400*                                                                         
041500     SKIP2                                                                
041600*                  *** ROMARK LEDTEXTER                                   
041700     SKIP3                                                                
041800   03  ROMARK-LEDTEXTER.                                                  
041900      07  FILLER     PIC X(01) VALUE 'R'.                                 
042000      07  FILLER     PIC X(01) VALUE 'B'.                                 
042100      07  FILLER     PIC X(01) VALUE 'B'.                                 
042200      07  FILLER     PIC X(01) VALUE 'B'.                                 
042300      07  FILLER     PIC X(01) VALUE 'B'.                                 
042400      07  FILLER     PIC X(01) VALUE 'B'.                                 
042500      07  FILLER     PIC X(01) VALUE 'B'.                                 
042600      07  FILLER     PIC X(01) VALUE 'B'.                                 
042700   03  FILLER  REDEFINES  ROMARK-LEDTEXTER.                               
042800      05  ROMARK-LEDTEXT  PIC X(01)  OCCURS 8 TIMES.                      
042900   03  FILLER  REDEFINES  ROMARK-LEDTEXTER.                               
043000      05  ROMARK-TEXT     PIC X(01)  OCCURS 8 TIMES.                      
043100     SKIP3                                                                
043200*                  *** RADREF LEDTEXTER                                   
043300*                                                                         
043400   03  BERADREF-LEDTEXTER.                                                
043500      07  FILLER     PIC X(11) VALUE 'RADREF.    '.                       
043600      07  FILLER     PIC X(11) VALUE 'ITEM REF.  '.                       
043700      07  FILLER     PIC X(11) VALUE 'REF.ARTICLE'.                       
043800      07  FILLER     PIC X(11) VALUE 'REF. DET.  '.                       
043900      07  FILLER     PIC X(11) VALUE 'ITEM REF.  '.                       
044000      07  FILLER     PIC X(11) VALUE 'RIF. LINEA '.                       
044100      07  FILLER     PIC X(11) VALUE 'ITEM REF.  '.                       
044200      07  FILLER     PIC X(11) VALUE 'ITEM REF.  '.                       
044300   03  FILLER  REDEFINES  BERADREF-LEDTEXTER.                             
044400      05  BERADREF-LEDTEXT  PIC X(11)  OCCURS 8 TIMES.                    
044500     SKIP3                                                                
044600*                  *** ART-NR LEDTEXTER                                   
044700*                                                                         
044800   03  IDARTNR-LEDTEXTER.                                                 
044900      07  FILLER     PIC X(09) VALUE 'ARTIKELNR'.                         
045000      07  FILLER     PIC X(09) VALUE '  PART NO'.                         
045100      07  FILLER     PIC X(09) VALUE '  ARTICLE'.                         
045200      07  FILLER     PIC X(09) VALUE '  DETALLE'.                         
045300      07  FILLER     PIC X(09) VALUE ' TEILE NR'.                         
045600      07  FILLER     PIC X(09) VALUE 'COD. ART.'.                         
045601      07  FILLER     PIC X(09) VALUE '  STYK NR'.                         
045610      07  FILLER     PIC X(09) VALUE '  PART NO'.                         
045700   03  FILLER  REDEFINES  IDARTNR-LEDTEXTER.                              
045800      05  IDARTNR-LEDTEXT  PIC X(09)  OCCURS 8 TIMES.                     
045900     SKIP3                                                                
046000*                  *** FLTILLK LEDTEXTER                                  
046100*                                                                         
046200   03  FLTILLK-LEDTEXTER.                                                 
046300      07  FILLER     PIC X(01) VALUE ' '.                                 
046400      07  FILLER     PIC X(01) VALUE ' '.                                 
046500      07  FILLER     PIC X(01) VALUE ' '.                                 
046600      07  FILLER     PIC X(01) VALUE ' '.                                 
046700      07  FILLER     PIC X(01) VALUE ' '.                                 
046800      07  FILLER     PIC X(01) VALUE ' '.                                 
046900      07  FILLER     PIC X(01) VALUE ' '.                                 
047000      07  FILLER     PIC X(01) VALUE ' '.                                 
047100   03  FILLER  REDEFINES  FLTILLK-LEDTEXTER.                              
047200      05  FLTILLK-LEDTEXT  PIC X(01)  OCCURS 8 TIMES.                     
047300     SKIP3                                                                
047400*                  *** FLTILLK TEXTER                                     
047500*                                                                         
047600   03  FLTILLK-TEXTER.                                                    
047700      07  FILLER     PIC X(01) VALUE '*'.                                 
047800      07  FILLER     PIC X(01) VALUE '*'.                                 
047900      07  FILLER     PIC X(01) VALUE '*'.                                 
048000      07  FILLER     PIC X(01) VALUE '*'.                                 
048100      07  FILLER     PIC X(01) VALUE '*'.                                 
048200      07  FILLER     PIC X(01) VALUE '*'.                                 
048300      07  FILLER     PIC X(01) VALUE '*'.                                 
048400      07  FILLER     PIC X(01) VALUE '*'.                                 
048500   03  FILLER  REDEFINES  FLTILLK-TEXTER.                                 
048600      05  FLTILLK-TEXT     PIC X(01)  OCCURS 8 TIMES.                     
048700     SKIP3                                                                
048800*                  *** ART-BENÄMNING LEDTEXTER                            
048900*                                                                         
049000   03  BEART-LEDTEXTER.                                                   
049100      07  FILLER     PIC X(12) VALUE 'BENÄMNING   '.                      
049200      07  FILLER     PIC X(12) VALUE 'PART NAME   '.                      
049300      07  FILLER     PIC X(12) VALUE 'DESIGNATION '.                      
049400      07  FILLER     PIC X(12) VALUE 'DESCRIPCION '.                      
049500      07  FILLER     PIC X(12) VALUE 'BENENNUNG   '.                      
049800      07  FILLER     PIC X(12) VALUE 'DES.ARTICOLO'.                      
049801      07  FILLER     PIC X(12) VALUE 'BENAMING    '.                      
049810      07  FILLER     PIC X(12) VALUE 'PART NAME   '.                      
049900   03  FILLER  REDEFINES  BEART-LEDTEXTER.                                
050000      05  BEART-LEDTEXT  PIC X(12)  OCCURS 8 TIMES.                       
050100     SKIP3                                                                
050200*                  *** KVANT BESTÄLLD LEDTEXTER                           
050300*                                                                         
050400   03  KVBEART-LEDTEXTER.                                                 
050500      07  FILLER     PIC X(06) VALUE ' B.ANT'.                            
050600      07  FILLER     PIC X(06) VALUE ' Q.REQ'.                            
050700      07  FILLER     PIC X(06) VALUE ' Q.COM'.                            
050800      07  FILLER     PIC X(06) VALUE 'C.SOLI'.                            
050900      07  FILLER     PIC X(06) VALUE 'B.MENG'.                            
051100      07  FILLER     PIC X(06) VALUE 'Q.RIC.'.                            
051110      07  FILLER     PIC X(06) VALUE 'BST.HV'.                            
051200      07  FILLER     PIC X(06) VALUE ' Q.REQ'.                            
051300   03  FILLER  REDEFINES  KVBEART-LEDTEXTER.                              
051400      05  KVBEART-LEDTEXT  PIC X(06)  OCCURS 8 TIMES.                     
051500     SKIP3                                                                
051600*                  *** LEVERERAT ANTAL LEDTEXTER                          
051700*                                                                         
051800   03  KVLEVART-LEDTEXTER.                                                
051900      07  FILLER     PIC X(06) VALUE ' L.ANT'.                            
052000      07  FILLER     PIC X(06) VALUE ' Q.DEL'.                            
052100      07  FILLER     PIC X(06) VALUE ' Q.LIV'.                            
052200      07  FILLER     PIC X(06) VALUE 'C.DESP'.                            
052300      07  FILLER     PIC X(06) VALUE 'G.MENG'.                            
052500      07  FILLER     PIC X(06) VALUE 'Q.SPED'.                            
052510      07  FILLER     PIC X(06) VALUE 'GEL.HV'.                            
052600      07  FILLER     PIC X(06) VALUE ' Q.DEL'.                            
052700   03  FILLER  REDEFINES  KVLEVART-LEDTEXTER.                             
052800      05  KVLEVART-LEDTEXT  PIC X(06)  OCCURS 8 TIMES.                    
052900     SKIP2                                                                
053000*                  *** FLSPLIT  LEDTEXTER                                 
053100*                                                                         
053200   03  FLSPLIT-LEDTEXTER.                                                 
053300      07  FILLER     PIC X(01) VALUE 'S'.                                 
053400      07  FILLER     PIC X(01) VALUE 'S'.                                 
053500      07  FILLER     PIC X(01) VALUE 'S'.                                 
053600      07  FILLER     PIC X(01) VALUE 'S'.                                 
053700      07  FILLER     PIC X(01) VALUE 'S'.                                 
053800      07  FILLER     PIC X(01) VALUE 'S'.                                 
053900      07  FILLER     PIC X(01) VALUE 'S'.                                 
054000      07  FILLER     PIC X(01) VALUE 'S'.                                 
054100   03  FILLER  REDEFINES  FLSPLIT-LEDTEXTER.                              
054200      05  FLSPLIT-LEDTEXT  PIC X(01)  OCCURS 8 TIMES.                     
054300   03  FILLER  REDEFINES  FLSPLIT-LEDTEXTER.                              
054400      05  FLSPLIT-TEXT     PIC X(01)  OCCURS 8 TIMES.                     
054500     SKIP2                                                                
054600*                  *** APRIS  LEDTEXTER                                   
054700*                                                                         
054800   03  PRARTNTO-LEDTEXTER.                                                
054900      07  FILLER     PIC X(10) VALUE ' STYCKPRIS'.                        
055000      07  FILLER     PIC X(10) VALUE 'UNIT PRICE'.                        
055100      07  FILLER     PIC X(10) VALUE 'PRIX UNIT.'.                        
055200      07  FILLER     PIC X(10) VALUE 'PRECIO UN.'.                        
055300      07  FILLER     PIC X(10) VALUE ' EINZELPR.'.                        
055500      07  FILLER     PIC X(10) VALUE 'P.UNITARIO'.                        
055510      07  FILLER     PIC X(10) VALUE ' EENHEIDPR'.                        
055600      07  FILLER     PIC X(10) VALUE 'UNIT PRICE'.                        
055700   03  FILLER  REDEFINES  PRARTNTO-LEDTEXTER.                             
055800      05  PRARTNTO-LEDTEXT  PIC X(10)  OCCURS 8 TIMES.                    
055900     SKIP3                                                                
056000*                  *** RADPRIS LEDTEXTER                                  
056100*                                                                         
056200   03  SUARTNTO-LEDTEXTER.                                                
056300      07  FILLER     PIC X(10) VALUE '   RADPRIS'.                        
056400      07  FILLER     PIC X(10) VALUE ' TOT.PRICE'.                        
056500      07  FILLER     PIC X(10) VALUE 'PRIX TOTAL'.                        
056600      07  FILLER     PIC X(10) VALUE '     TOTAL'.                        
056700      07  FILLER     PIC X(10) VALUE ' GES.PREIS'.                        
056900      07  FILLER     PIC X(10) VALUE 'PREZZO TOT'.                        
056910      07  FILLER     PIC X(10) VALUE 'TOTALPRIJS'.                        
057000      07  FILLER     PIC X(10) VALUE ' TOT.PRICE'.                        
057100   03  FILLER  REDEFINES  SUARTNTO-LEDTEXTER.                             
057200      05  SUARTNTO-LEDTEXT  PIC X(10)  OCCURS 8 TIMES.                    
057300     SKIP3                                                                
057400*                  *** BEARTURS LEDTEXTER                                 
057500*                                                                         
057600   03  BEARTURS-LEDTEXTER.                                                
057700      07  FILLER     PIC X(03) VALUE 'URS'.                               
057800      07  FILLER     PIC X(03) VALUE 'ORG'.                               
057900      07  FILLER     PIC X(03) VALUE 'ORG'.                               
058000      07  FILLER     PIC X(03) VALUE 'ORG'.                               
058100      07  FILLER     PIC X(03) VALUE 'URS'.                               
058300      07  FILLER     PIC X(03) VALUE 'OR.'.                               
058310      07  FILLER     PIC X(03) VALUE 'URS'.                               
058400      07  FILLER     PIC X(03) VALUE 'ORG'.                               
058500   03  FILLER  REDEFINES  BEARTURS-LEDTEXTER.                             
058600      05  BEARTURS-LEDTEXT  PIC X(03)  OCCURS 8 TIMES.                    
058700     SKIP3                                                                
058800*                  *** KDSRA  LEDTEXTER                                   
058900*                                                                         
059000   03  KDSRA-LEDTEXTER.                                                   
059100      07  FILLER     PIC X(03) VALUE 'SRA'.                               
059200      07  FILLER     PIC X(03) VALUE 'SRA'.                               
059300      07  FILLER     PIC X(03) VALUE 'SRA'.                               
059400      07  FILLER     PIC X(03) VALUE 'SRA'.                               
059500      07  FILLER     PIC X(03) VALUE 'SRA'.                               
059600      07  FILLER     PIC X(03) VALUE 'SRA'.                               
059700      07  FILLER     PIC X(03) VALUE 'SRA'.                               
059800      07  FILLER     PIC X(03) VALUE 'SRA'.                               
059900   03  FILLER  REDEFINES  KDSRA-LEDTEXTER.                                
060000      05  KDSRA-LEDTEXT     PIC X(03)  OCCURS 8 TIMES.                    
060100     SKIP3                                                                
060200*                  *** KDTVA  LEDTEXTER                                   
060300*                                                                         
060400   03  KDTVA-LEDTEXTER.                                                   
060500      07  FILLER     PIC X(02) VALUE ' V'.                                
060600      07  FILLER     PIC X(02) VALUE ' V'.                                
060700      07  FILLER     PIC X(02) VALUE ' T'.                                
060800      07  FILLER     PIC X(02) VALUE ' V'.                                
060900      07  FILLER     PIC X(02) VALUE ' V'.                                
061000      07  FILLER     PIC X(02) VALUE ' I'.                                
061100      07  FILLER     PIC X(02) VALUE ' V'.                                
061200      07  FILLER     PIC X(02) VALUE ' V'.                                
061300   03  FILLER  REDEFINES  KDTVA-LEDTEXTER.                                
061400      05  KDTVA-LEDTEXT     PIC X(02)  OCCURS 8 TIMES.                    
061500     SKIP3                                                                
061600*                  *** NETTOVIKT LEDTEXTER                                
061700*                                                                         
061800   03  VKARTNTO-LEDTEXTER.                                                
061900      07  FILLER     PIC X(07) VALUE 'NTOVIKT'.                           
062000      07  FILLER     PIC X(07) VALUE 'NET WGT'.                           
062100      07  FILLER     PIC X(07) VALUE 'PDS NET'.                           
062200      07  FILLER     PIC X(07) VALUE 'PESO NT'.                           
062300      07  FILLER     PIC X(07) VALUE 'GEWICHT'.                           
062500      07  FILLER     PIC X(07) VALUE 'P/NETTO'.                           
062510      07  FILLER     PIC X(07) VALUE 'GEWICHT'.                           
062600      07  FILLER     PIC X(07) VALUE 'NET WGT'.                           
062700   03  FILLER  REDEFINES  VKARTNTO-LEDTEXTER.                             
062800      05  VKARTNTO-LEDTEXT  PIC X(07)  OCCURS 8 TIMES.                    
062900     SKIP3                                                                
063000*                  *** STAT-NR  LEDTEXTER                                 
063100*                                                                         
063200   03  IDSTATNR-LEDTEXTER.                                                
063300      07  FILLER     PIC X(09) VALUE 'STAT.NR.'.                          
063400      07  FILLER     PIC X(09) VALUE 'STAT.NO.'.                          
063500      07  FILLER     PIC X(09) VALUE 'NO STAT '.                          
063600      07  FILLER     PIC X(09) VALUE 'N.ESTAD '.                          
063700      07  FILLER     PIC X(09) VALUE 'STAT NR '.                          
063900      07  FILLER     PIC X(09) VALUE 'RIF.STAT'.                          
063910      07  FILLER     PIC X(09) VALUE 'STAT NR '.                          
064000      07  FILLER     PIC X(09) VALUE 'STAT.NO.'.                          
064100   03  FILLER  REDEFINES  IDSTATNR-LEDTEXTER.                             
064200      05  IDSTATNR-LEDTEXT  PIC X(09)  OCCURS 8 TIMES.                    
064300*                                                                         
064400     EJECT                                                                
064500*                  *** PALL     LEDTEXTER                                 
064600*                                                                         
064700   03  KDPALL-LEDTEXTER.                                                  
064800      07  FILLER     PIC X(09) VALUE 'PALL     '.                         
064900      07  FILLER     PIC X(09) VALUE 'PALL.    '.                         
065000      07  FILLER     PIC X(09) VALUE 'PALET    '.                         
065100      07  FILLER     PIC X(09) VALUE 'BANQ.    '.                         
065200      07  FILLER     PIC X(09) VALUE 'PALET    '.                         
065400      07  FILLER     PIC X(09) VALUE 'PALLET   '.                         
065410      07  FILLER     PIC X(09) VALUE 'PALET    '.                         
065500      07  FILLER     PIC X(09) VALUE 'PALL.    '.                         
065600   03  FILLER  REDEFINES  KDPALL-LEDTEXTER.                               
065700      05  KDPALL-LEDTEXT  PIC X(09)  OCCURS 8 TIMES.                      
065800*                                                                         
065900     SKIP3                                                                
066000*                  *** ANTAL KRAGAR LEDTEXTER                             
066100*                                                                         
066200   03  KVKRAG-LEDTEXTER.                                                  
066300      07  FILLER     PIC X(09) VALUE 'KRAGAR   '.                         
066400      07  FILLER     PIC X(09) VALUE 'FRAMES   '.                         
066500      07  FILLER     PIC X(09) VALUE 'REHAUSSES'.                         
066600      07  FILLER     PIC X(09) VALUE 'MARCO    '.                         
066700      07  FILLER     PIC X(09) VALUE 'RAHMEN   '.                         
066900      07  FILLER     PIC X(09) VALUE 'STRUTTURA'.                         
066910      07  FILLER     PIC X(09) VALUE 'STAT NR '.                          
067000      07  FILLER     PIC X(09) VALUE 'FRAMES   '.                         
067100   03  FILLER  REDEFINES  KVKRAG-LEDTEXTER.                               
067200      05  KVKRAG-LEDTEXT  PIC X(09)  OCCURS 8 TIMES.                      
067300*                                                                         
067400     EJECT                                                                
067500*                  *** ANTAL LOCK   LEDTEXTER                             
067600*                                                                         
067700   03  KVLOCK-LEDTEXTER.                                                  
067800      07  FILLER     PIC X(09) VALUE 'LOCK     '.                         
067900      07  FILLER     PIC X(09) VALUE 'LIDS     '.                         
068000      07  FILLER     PIC X(09) VALUE 'COUVERCLE'.                         
068100      07  FILLER     PIC X(09) VALUE 'TAPA     '.                         
068200      07  FILLER     PIC X(09) VALUE 'DECKEL   '.                         
068400      07  FILLER     PIC X(09) VALUE 'COPERCHIO'.                         
068410      07  FILLER     PIC X(09) VALUE 'DEKSELR '.                          
068500      07  FILLER     PIC X(09) VALUE 'LIDS     '.                         
068600   03  FILLER  REDEFINES  KVLOCK-LEDTEXTER.                               
068700      05  KVLOCK-LEDTEXT  PIC X(09)  OCCURS 8 TIMES.                      
068800*                                                                         
068900*                  *** KOLLIKODER LEDTEXTER                               
069000*                                                                         
069100   03  KDKOLLI-LEDTEXTER.                                                 
069200      07  FILLER     PIC X(09) VALUE 'KOLLIKOD '.                         
069300      07  FILLER     PIC X(09) VALUE 'CASE CODE'.                         
069400      07  FILLER     PIC X(09) VALUE 'CASE CODE'.                         
069500      07  FILLER     PIC X(09) VALUE 'CASE CODE'.                         
069600      07  FILLER     PIC X(09) VALUE 'CASE CODE'.                         
069700      07  FILLER     PIC X(09) VALUE 'COD.CASSA'.                         
069800      07  FILLER     PIC X(09) VALUE 'CASE CODE'.                         
069900      07  FILLER     PIC X(09) VALUE 'CASE CODE'.                         
070000   03  FILLER  REDEFINES  KDKOLLI-LEDTEXTER.                              
070100      05  KDKOLLI-LEDTEXT  PIC X(09)  OCCURS 8 TIMES.                     
070200*                                                                         
070300*                  *** KOLLIKODER LEDTEXTER                               
070400*                                                                         
070500   03  VKTARA-LEDTEXTER.                                                  
070600      07  FILLER     PIC X(08) VALUE '    TARA'.                          
070700      07  FILLER     PIC X(08) VALUE '    TARE'.                          
070800      07  FILLER     PIC X(08) VALUE '    TARE'.                          
070900      07  FILLER     PIC X(08) VALUE '    TARE'.                          
071000      07  FILLER     PIC X(08) VALUE '    TARE'.                          
071100      07  FILLER     PIC X(08) VALUE '    TARA'.                          
071110      07  FILLER     PIC X(08) VALUE '    TARE'.                          
071120      07  FILLER     PIC X(08) VALUE '    TARE'.                          
071200   03  FILLER  REDEFINES  VKTARA-LEDTEXTER.                               
071300      05  VKTARA-LEDTEXT  PIC X(08)  OCCURS 8 TIMES.                      
071400*                                                                         
071500*** END COPY W475W552    LENGTH=      OLD LENGTH=                         
