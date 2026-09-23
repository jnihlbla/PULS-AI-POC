000010*** EDIT ALLOWED                                                          
000100 01  W475W551.                                                            
000200*                                                                         
000300*    FAKTURA OCH PROFORMA HUVUD LEDTEXTER I 8 MÖJLIGA SPRÅK :             
000400*                                                                         
000500*        1 - SVENSKA                                                      
000600*        2 - ENGELSKA                                                     
000700*        3 - FRANSKA                                                      
000800*        4 - SPANSKA                                                      
000900*        5 - TYSKA                                                        
001010*        6 - ITALIENSKA                                                   
001011*        7 - FLAMLÄNDSKA                                                  
001020*        8 - LEDIGT                                                       
001100*                                                                         
001200*            ***  SPECIAL MARKERING TEXTER                                
001300*                                                                         
001400  03  P-KDFAKTYP-TEXTER.                                                  
001500    05 FILLER PIC X(33) VALUE '                         PROFORMA'.        
001600    05 FILLER PIC X(33) VALUE '                         PROFORMA'.        
001700    05 FILLER PIC X(33) VALUE '                         PROFORMA'.        
001800    05 FILLER PIC X(33) VALUE '                         PROFORMA'.        
001900    05 FILLER PIC X(33) VALUE '                         PROFORMA'.        
002000    05 FILLER PIC X(33) VALUE '                         PROFORMA'.        
002010    05 FILLER PIC X(33) VALUE '                         PROFORMA'.        
002020    05 FILLER PIC X(33) VALUE '                         PROFORMA'.        
002100*                                                                         
002200  03  FILLER  REDEFINES  P-KDFAKTYP-TEXTER.                               
002300    05  P-KDFAKTYP-TEXT           PIC X(33)   OCCURS 8 TIMES.             
002400     SKIP2                                                                
003800*            ***  FRAKT SÄTT  LEDTEXTER                                   
003900*                                                                         
004000  03  BEFRAKT-LEDTEXTER.                                                  
004100    05 FILLER PIC X(30) VALUE 'FRAKTSÄTT                     '.           
004200    05 FILLER PIC X(30) VALUE 'FREIGHT MODE                  '.           
004300    05 FILLER PIC X(30) VALUE 'MODE DE TRANSPORT             '.           
004400    05 FILLER PIC X(30) VALUE 'TRANSPORTE                    '.           
004500    05 FILLER PIC X(30) VALUE 'FRACHT                        '.           
004610    05 FILLER PIC X(30) VALUE 'MODALITA  DI TRANSPORTO       '.           
004611    05 FILLER PIC X(30) VALUE 'VRACHT                        '.           
004620    05 FILLER PIC X(30) VALUE 'FREIGHT MODE                  '.           
004700*                                                                         
004800  03  FILLER  REDEFINES  BEFRAKT-LEDTEXTER.                               
004900    05  BEFRAKT-LEDTEXT           PIC X(30)   OCCURS  8 TIMES.            
005000     SKIP2                                                                
005100*            ***  LEVERANSVILLKOR  LEDTEXTER                              
005200*                                                                         
005300  03  BELEVVIL-LEDTEXTER.                                                 
005400    05 FILLER PIC X(30) VALUE 'LEVERANSVILLKOR               '.           
005500    05 FILLER PIC X(30) VALUE 'DELIVERY TERMS                '.           
005600    05 FILLER PIC X(30) VALUE 'CONDITIONS DE LIVRAISON       '.           
005700    05 FILLER PIC X(30) VALUE 'CONDICION DE DESPACHO         '.           
005800    05 FILLER PIC X(30) VALUE 'LIEFERUNGSBEDINGUNG           '.           
005910    05 FILLER PIC X(30) VALUE 'CONDIZIONI DI PAGAMENTO       '.           
005911    05 FILLER PIC X(30) VALUE 'LEVERINGSVOORWAARDEN          '.           
005920    05 FILLER PIC X(30) VALUE 'DELIVERY TERMS                '.           
006000*                                                                         
006100  03  FILLER  REDEFINES  BELEVVIL-LEDTEXTER.                              
006200    05  BELEVVIL-LEDTEXT           PIC X(30)   OCCURS  8 TIMES.           
006300     SKIP2                                                                
006400*            ***  BOKNINGSNUMMER  LEDTEXTER                               
006500*                                                                         
006600  03  IDBOKN-LEDTEXTER.                                                   
006700    05 FILLER PIC X(30) VALUE 'BOKNINGSNUMMER                '.           
006800    05 FILLER PIC X(30) VALUE 'BOOKING NUMBER                '.           
006900    05 FILLER PIC X(30) VALUE 'NUMERO DE RESERVATION         '.           
007000    05 FILLER PIC X(30) VALUE 'BOOKING NUMBER                '.           
007100    05 FILLER PIC X(30) VALUE 'RESERVIERUNGSNR               '.           
007210    05 FILLER PIC X(30) VALUE 'RISERVATO NUMERO              '.           
007211    05 FILLER PIC X(30) VALUE 'RESERVATIE-NR                 '.           
007220    05 FILLER PIC X(30) VALUE 'BOOKING NUMBER                '.           
007300*                                                                         
007400  03  FILLER  REDEFINES  IDBOKN-LEDTEXTER.                                
007500    05  IDBOKN-LEDTEXT           PIC X(30)   OCCURS  8 TIMES.             
007600     SKIP2                                                                
007700*            ***  LC-NUMMER  LEDTEXTER                                    
007800*                                                                         
007900  03  IDLC-LEDTEXTER.                                                     
008000    05 FILLER PIC X(30) VALUE 'LC-NUMMER                     '.           
008100    05 FILLER PIC X(30) VALUE 'LETTER OF CREDIT NO           '.           
008200    05 FILLER PIC X(30) VALUE 'NUMERO LETTRE DE CREDIT       '.           
008300    05 FILLER PIC X(30) VALUE 'NUMERO DE L/C                 '.           
008400    05 FILLER PIC X(30) VALUE 'KREDIETBRIEF NR               '.           
008510    05 FILLER PIC X(30) VALUE 'LETTERA DI CREDITO N.         '.           
008511    05 FILLER PIC X(30) VALUE 'KREDIETBRIEF NR               '.           
008520    05 FILLER PIC X(30) VALUE 'LETTER OF CREDIT NR           '.           
008600*                                                                         
008700  03  FILLER  REDEFINES  IDLC-LEDTEXTER.                                  
008800    05  IDLC-LEDTEXT           PIC X(30)   OCCURS  8 TIMES.               
008900     SKIP2                                                                
009000*            ***  BETALNINGSVILLKORS  LEDTEXTER                           
009100*                                                                         
009200  03  BEBETVIL-LEDTEXTER.                                                 
009300    05 FILLER PIC X(30) VALUE 'BETALNINGSVILLKOR             '.           
009400    05 FILLER PIC X(30) VALUE 'PAYMENT TERMS                 '.           
009500    05 FILLER PIC X(30) VALUE 'CONDITIONS DE PAIEMENT        '.           
009600    05 FILLER PIC X(30) VALUE 'CONDICION DE PAGO             '.           
009700    05 FILLER PIC X(30) VALUE 'ZAHLUNGSBEDINGUNGEN           '.           
009810    05 FILLER PIC X(30) VALUE 'TERMINI DI PAGAMENTO          '.           
009811    05 FILLER PIC X(30) VALUE 'BETALNINGSVOORWAARDEN         '.           
009820    05 FILLER PIC X(30) VALUE 'PAYMENT TERMS                 '.           
009900*                                                                         
010000  03  FILLER  REDEFINES  BEBETVIL-LEDTEXTER.                              
010100    05  BEBETVIL-LEDTEXT           PIC X(30)   OCCURS  8 TIMES.           
010200     SKIP2                                                                
010300*            ***  LICENSNUMMER  LEDTEXTER                                 
010400*                                                                         
010500  03  IDLICENS-LEDTEXTER.                                                 
010600    05 FILLER PIC X(30) VALUE 'LICENSNUMMER                  '.           
010700    05 FILLER PIC X(30) VALUE 'LICENCE NUMBER                '.           
010800    05 FILLER PIC X(30) VALUE 'NUMERO DE LICENCE             '.           
010900    05 FILLER PIC X(30) VALUE 'NRO DE LICENCIA               '.           
011000    05 FILLER PIC X(30) VALUE 'LIZENZNR                      '.           
011110    05 FILLER PIC X(30) VALUE 'NUMERO DI LICENZA             '.           
011111    05 FILLER PIC X(30) VALUE 'LICENTIE-NR                   '.           
011120    05 FILLER PIC X(30) VALUE 'LICENCE NUMBER                '.           
011200*                                                                         
011300  03  FILLER  REDEFINES  IDLICENS-LEDTEXTER.                              
011400    05  IDLICENS-LEDTEXT           PIC X(30)   OCCURS  8 TIMES.           
011500     SKIP2                                                                
011600*            ***  KONTRAKTSNUMMER  LEDTEXTER                              
011700*                                                                         
011800  03  IDKONTR-LEDTEXTER.                                                  
011900    05 FILLER PIC X(22) VALUE 'KONTRAKTSNUMMER       '.                   
012000    05 FILLER PIC X(22) VALUE 'CONTRACT NUMBER       '.                   
012100    05 FILLER PIC X(22) VALUE 'NUMERO DE CONTRAT     '.                   
012200    05 FILLER PIC X(22) VALUE 'NRO DE CONTRATO       '.                   
012300    05 FILLER PIC X(22) VALUE 'KONTRAKTNUMMER        '.                   
012410    05 FILLER PIC X(22) VALUE 'CONTRATTO NUMERO      '.                   
012411    05 FILLER PIC X(22) VALUE 'CONTRACT-NR           '.                   
012420    05 FILLER PIC X(22) VALUE 'CONTRACT NUMBER       '.                   
012500*                                                                         
012600  03  FILLER  REDEFINES  IDKONTR-LEDTEXTER.                               
012700    05  IDKONTR-LEDTEXT           PIC X(22)   OCCURS  8 TIMES.            
012800     SKIP2                                                                
012900*            ***  FRAKTSEDELNUMMER  LEDTEXTER                             
013000*                                                                         
013100  03  IDFRASED-LEDTEXTER.                                                 
013200    05 FILLER PIC X(22) VALUE 'FRAKTSEDELNUMMER      '.                   
013300    05 FILLER PIC X(22) VALUE 'FREIGHTBILL NUMBER    '.                   
013400    05 FILLER PIC X(11) VALUE 'BORDEREAU D'.                              
013500    05 FILLER PIC X(01) VALUE QUOTE.                                      
013600    05 FILLER PIC X(10) VALUE 'EXPEDITION'.                               
013700    05 FILLER PIC X(22) VALUE 'NRO DE GUIA TRANSP    '.                   
013800    05 FILLER PIC X(22) VALUE 'LIEFERSCHEINNR        '.                   
013910    05 FILLER PIC X(22) VALUE 'LETTERA DI VETTURA    '.                   
013911    05 FILLER PIC X(22) VALUE 'VRACHT-BRIEF-NR       '.                   
013920    05 FILLER PIC X(22) VALUE 'FREIGHTBILL NUMBER    '.                   
014000*                                                                         
014100  03  FILLER  REDEFINES  IDFRASED-LEDTEXTER.                              
014200    05  IDFRASED-LEDTEXT           PIC X(22)   OCCURS  8 TIMES.           
014300     SKIP2                                                                
014400*            ***  VARUCERTIFICATNR  LEDTEXTER                             
014500*                                                                         
014600  03  IDVCERT-LEDTEXTER.                                                  
014700    05 FILLER PIC X(22) VALUE 'VARUCERTIFICATNR      '.                   
014800    05 FILLER PIC X(22) VALUE 'MOVEMENT CERTIFICATE  '.                   
014900    05 FILLER PIC X(22) VALUE 'CERT.CIRC.MARCHANDISES'.                   
015000    05 FILLER PIC X(22) VALUE 'NRO DE CERTIFICADO    '.                   
015100    05 FILLER PIC X(22) VALUE 'WARENVERKEHRSCHEIN    '.                   
015210    05 FILLER PIC X(22) VALUE 'CERT.DI MOVIMENTAZIONE'.                   
015211    05 FILLER PIC X(22) VALUE 'EUR.1 - CERTIFICAAT   '.                   
015220    05 FILLER PIC X(22) VALUE 'MOVEMENT CERTIFICATE  '.                   
015300*                                                                         
015400  03  FILLER  REDEFINES  IDVCERT-LEDTEXTER.                               
015500    05  IDVCERT-LEDTEXT           PIC X(22)   OCCURS  8 TIMES.            
015600     SKIP2                                                                
015700*            ***  SKEPPNINGSNUMMER  LEDTEXTER                             
015800*                                                                         
015900  03  IDSKEPPN-LEDTEXTER.                                                 
016000    05 FILLER PIC X(12) VALUE 'SKEPPNINGSNR'.                             
016100    05 FILLER PIC X(12) VALUE 'SHIPPING NO '.                             
016200    05 FILLER PIC X(12) VALUE 'NUMERO ENVOI'.                             
016300    05 FILLER PIC X(12) VALUE 'REF. TRANS. '.                             
016400    05 FILLER PIC X(12) VALUE 'VERSANDSNR  '.                             
016510    05 FILLER PIC X(12) VALUE 'RIFERIMENTO '.                             
016511    05 FILLER PIC X(12) VALUE 'VERSCHEPNING'.                             
016520    05 FILLER PIC X(12) VALUE 'SHIPPING NO '.                             
016600*                                                                         
016700  03  FILLER  REDEFINES  IDSKEPPN-LEDTEXTER.                              
016800    05  IDSKEPPN-LEDTEXT      PIC X(12)   OCCURS  8 TIMES.                
016900     SKIP2                                                                
017000*            ***  REFERENS TILL OFFERT LEDTEXTER                          
017100*                                                                         
017200  03  BEOFFREF-LEDTEXTER.                                                 
017300    05 FILLER PIC X(20) VALUE 'OFFERT REFERENS     '.                     
017400    05 FILLER PIC X(20) VALUE 'QUOTATION REFERENCE '.                     
017500    05 FILLER PIC X(14) VALUE 'REFERENCE DE L'.                           
017600    05 FILLER PIC X(01) VALUE QUOTE.                                      
017700    05 FILLER PIC X(05) VALUE 'OFFRE'.                                    
017800    05 FILLER PIC X(20) VALUE 'REF DE COTIZATION   '.                     
017900    05 FILLER PIC X(20) VALUE 'ANGEBOTNUMMER       '.                     
018010    05 FILLER PIC X(20) VALUE 'QUOTATION REFERENCE '.                     
018011    05 FILLER PIC X(20) VALUE 'OFFERTE NUMMER      '.                     
018020    05 FILLER PIC X(20) VALUE 'QUOTATION REFERENCE '.                     
018100*                                                                         
018200  03  FILLER  REDEFINES  BEOFFREF-LEDTEXTER.                              
018300    05  BEOFFREF-LEDTEXT           PIC X(20)   OCCURS  8 TIMES.           
018400     SKIP2                                                                
018500*            ***  BRUTTOVIKT      LEDTEXTER                               
018600*                                                                         
018700  03  HUV-VKORDBTO-LEDTEXTER.                                             
018800    05 FILLER PIC X(14) VALUE 'BRUTTOVIKT  KG'.                           
018900    05 FILLER PIC X(14) VALUE 'GROSS WEIGHT  '.                           
019000    05 FILLER PIC X(14) VALUE 'POIDS BRUT    '.                           
019100    05 FILLER PIC X(14) VALUE 'PESO BRUTO    '.                           
019200    05 FILLER PIC X(14) VALUE 'BRUTTO GEWICHT'.                           
019310    05 FILLER PIC X(14) VALUE 'PESO LORDO    '.                           
019311    05 FILLER PIC X(14) VALUE 'BRUTTO GEWICHT'.                           
019320    05 FILLER PIC X(14) VALUE 'GROSS WEIGHT  '.                           
019400*                                                                         
019500  03  FILLER  REDEFINES  HUV-VKORDBTO-LEDTEXTER.                          
019600    05  HUV-VKORDBTO-LEDTEXT       PIC X(14)   OCCURS  8 TIMES.           
019700     SKIP2                                                                
019800*            ***  NETTOVIKT      LEDTEXTER                                
019900*                                                                         
020000  03  HUV-VKORDNTO-LEDTEXTER.                                             
020100    05 FILLER PIC X(14) VALUE '  NETTOVIKT KG'.                           
020200    05 FILLER PIC X(14) VALUE '    NET WEIGHT'.                           
020300    05 FILLER PIC X(14) VALUE '    POIDS NET '.                           
020400    05 FILLER PIC X(14) VALUE '    PESO NETO '.                           
020500    05 FILLER PIC X(14) VALUE ' NETTO GEWICHT'.                           
020610    05 FILLER PIC X(14) VALUE '    PESO NETTO'.                           
020611    05 FILLER PIC X(14) VALUE ' NETTO GEWICHT'.                           
020620    05 FILLER PIC X(14) VALUE '    NET WEIGHT'.                           
020700*                                                                         
020800  03  FILLER  REDEFINES  HUV-VKORDNTO-LEDTEXTER.                          
020900    05  HUV-VKORDNTO-LEDTEXT       PIC X(14)   OCCURS  8 TIMES.           
021000     SKIP2                                                                
021100*            ***  BRUTTOVOLYM      LEDTEXTER                              
021200*                                                                         
021300  03  HUV-VLORDBTO-LEDTEXTER.                                             
021400    05 FILLER PIC X(14) VALUE '       VOLYM  '.                           
021500    05 FILLER PIC X(14) VALUE '       VOLUME '.                           
021600    05 FILLER PIC X(14) VALUE '       VOLUME '.                           
021700    05 FILLER PIC X(14) VALUE '       VOLUME '.                           
021800    05 FILLER PIC X(14) VALUE '       VOLUMEN'.                           
021900    05 FILLER PIC X(14) VALUE '       VOLUME '.                           
021910    05 FILLER PIC X(14) VALUE '       VOLUME '.                           
021920    05 FILLER PIC X(14) VALUE '       VOLUME '.                           
022000*                                                                         
022100  03  FILLER  REDEFINES  HUV-VLORDBTO-LEDTEXTER.                          
022200    05  HUV-VLORDBTO-LEDTEXT       PIC X(14)   OCCURS  8 TIMES.           
022300     SKIP2                                                                
022400*            ***  ORDERVÄRDE  LEDTEXTER                                   
022500*                                                                         
022600  03  HUV-SUFKTBEL-LEDTEXTER.                                             
022700    05 FILLER PIC X(19) VALUE '  FAKTURA VÄRDE SEK'.                      
022800    05 FILLER PIC X(19) VALUE '    TOTAL VALUE SEK'.                      
022900    05 FILLER PIC X(19) VALUE 'VALEUR TOTAL TC SEK'.                      
023000    05 FILLER PIC X(19) VALUE '    VALOR TOTAL SEK'.                      
023100    05 FILLER PIC X(19) VALUE '     WERT TOTAL SEK'.                      
023210    05 FILLER PIC X(19) VALUE '  TOTALE VALORE SEK'.                      
023211    05 FILLER PIC X(19) VALUE '   WAARDE TOTAL SEK'.                      
023220    05 FILLER PIC X(19) VALUE '    TOTAL VALUE SEK'.                      
023300*                                                                         
023400  03  FILLER  REDEFINES  HUV-SUFKTBEL-LEDTEXTER.                          
023500    05  HUV-SUFKTBEL-LEDTEXT     PIC X(19)   OCCURS  8 TIMES.             
023600     SKIP2                                                                
023700*            ***  FÖRSÄKRAN ENL KONTRAKT  LEDTEXTER                       
023800*                                                                         
023900  03  BEFORSKN-KONTR-LEDTEXTER.                                           
024000    05 FILLER PIC X(30) VALUE 'FÖRSÄKRAN ENL. KONTRAKT/LC    '.           
024100    05 FILLER PIC X(30) VALUE 'INSURANCE ACC.CONTRACT/LC     '.           
024200    05 FILLER PIC X(11) VALUE 'ASSURANCE D'.                              
024300    05 FILLER PIC X(01) VALUE QUOTE.                                      
024400    05 FILLER PIC X(18) VALUE 'APRES CONTRAT/LC  '.                       
024500    05 FILLER PIC X(30) VALUE 'INDICACION SEGUN CONTRATO-L/C '.           
024600    05 FILLER PIC X(30) VALUE 'VERSICHERUNG LAUT KONTRAKT/KB '.           
024710    05 FILLER PIC X(30) VALUE 'CONTRATTO DI ASSICURATIONE N. '.           
024711    05 FILLER PIC X(30) VALUE 'VERZEKERING VOLGENS CONTR./KB '.           
024720    05 FILLER PIC X(30) VALUE 'INSURANCE ACC.CONTRACT/LC     '.           
024800*                                                                         
024900  03  FILLER  REDEFINES  BEFORSKN-KONTR-LEDTEXTER.                        
025000    05  BEFORSKN-KONTR-LEDTEXT   PIC X(30)   OCCURS  8 TIMES.             
025100     SKIP2                                                                
025200*            ***  GODSMÄRKNING  LEDTEXTER                                 
025300*                                                                         
025400  03  BEGDSMRK-LEDTEXTER.                                                 
025500    05 FILLER PIC X(30) VALUE 'GODSMÄRKE                     '.           
025600    05 FILLER PIC X(30) VALUE 'GOODS MARKING                 '.           
025700    05 FILLER PIC X(30) VALUE 'MARQUAGE DES MARCHANDISES     '.           
025800    05 FILLER PIC X(30) VALUE 'MARCA                         '.           
025900    05 FILLER PIC X(30) VALUE 'MERKZEICHEN                   '.           
026010    05 FILLER PIC X(30) VALUE 'MARCHIO                       '.           
026011    05 FILLER PIC X(30) VALUE 'MERKZEN                       '.           
026020    05 FILLER PIC X(30) VALUE 'GOODS MARKING                 '.           
026100*                                                                         
026200  03  FILLER  REDEFINES   BEGDSMRK-LEDTEXTER.                             
026300    05   BEGDSMRK-LEDTEXT          PIC X(30)   OCCURS  8 TIMES.           
026400     SKIP2                                                                
026410*            ***  MOMS REGNR    LEDTEXTER                                 
026420*                                                                         
026430  03  BEVAT-NL-LEDTEXTER.                                                 
026450    05 FILLER PIC X(36) VALUE                                             
026451                        'VAT REGISTRATION NO.  : DISPATCHER  '.           
026452    05 FILLER PIC X(36) VALUE                                             
026453                        'VAT REGISTRATION NO.  : DISPATCHER  '.           
026454    05 FILLER PIC X(36) VALUE                                             
026455                        'NO. IDENTIFICATION TVA: FOURNISSEUR '.           
026456    05 FILLER PIC X(36) VALUE                                             
026457                        'VAT REGISTRATION NO.  : DISPATCHER  '.           
026458    05 FILLER PIC X(36) VALUE                                             
026459                        'VAT REGISTRATION NO.  : DISPATCHER  '.           
026460    05 FILLER PIC X(36) VALUE                                             
026470                        'P. IVA NUMERO         : MITTENTE    '.           
026480    05 FILLER PIC X(36) VALUE                                             
026490                        'VAT REGISTRATION NO.  : DISPATCHER  '.           
026491    05 FILLER PIC X(36) VALUE                                             
026492                        'VAT REGISTRATION NO.  : DISPATCHER  '.           
026493*                                                                         
026494  03  FILLER  REDEFINES   BEVAT-NL-LEDTEXTER.                             
026495    05   BEVAT-NL-LEDTEXT          PIC X(36)   OCCURS  8 TIMES.           
026496     SKIP2                                                                
026497*            ***  MOMS REGNR    LEDTEXTER                                 
026498*                                                                         
026499  03  BEVAT-LEDTEXTER.                                                    
026500    05 FILLER PIC X(36) VALUE                                             
026501                        '                        RECEIVER    '.           
026502    05 FILLER PIC X(36) VALUE                                             
026503                        '                        RECEIVER    '.           
026504    05 FILLER PIC X(36) VALUE                                             
026505                        '                        CLIENT      '.           
026506    05 FILLER PIC X(36) VALUE                                             
026507                        '                        RECEIVER    '.           
026508    05 FILLER PIC X(36) VALUE                                             
026509                        '                        RECEIVER    '.           
026510    05 FILLER PIC X(36) VALUE                                             
026520                        '                        RICEVENTE   '.           
026530    05 FILLER PIC X(36) VALUE                                             
026531                        '                        RECEIVER    '.           
026532    05 FILLER PIC X(36) VALUE                                             
026533                        '                        RECEIVER    '.           
026540*                                                                         
026550  03  FILLER  REDEFINES   BEVAT-LEDTEXTER.                                
026560    05   BEVAT-LEDTEXT             PIC X(36)   OCCURS  8 TIMES.           
026570     SKIP2                                                                
026580*            ***  INTRA...NR    LEDTEXTER                                 
026590*                                                                         
026591  03  VAT1-LEDTEXTER.                                                     
026592    05 FILLER PIC X(42)                                                   
026593              VALUE 'INTRA-COMMUNITY SUPPLY FREE OF DUTCH VAT  '.         
026594    05 FILLER PIC X(42)                                                   
026595              VALUE 'INTRA-COMMUNITY SUPPLY FREE OF DUTCH VAT  '.         
026596    05 FILLER PIC X(42)                                                   
026597              VALUE 'INTRA-COMMUNITY SUPPLY FREE OF DUTCH VAT  '.         
026598    05 FILLER PIC X(42)                                                   
026599              VALUE 'INTRA-COMMUNITY SUPPLY FREE OF DUTCH VAT  '.         
026600    05 FILLER PIC X(42)                                                   
026601              VALUE 'INTRA-COMMUNITY SUPPLY FREE OF DUTCH VAT  '.         
026602    05 FILLER PIC X(42)                                                   
026603              VALUE 'INTRA-COMMUNITY SUPPLY FREE OF DUTCH VAT  '.         
026604    05 FILLER PIC X(42)                                                   
026605              VALUE 'INTRA-COMMUNITY SUPPLY FREE OF DUTCH VAT  '.         
026606    05 FILLER PIC X(42)                                                   
026607              VALUE 'INTRA-COMMUNITY SUPPLY FREE OF DUTCH VAT  '.         
026608*                                                                         
026609  03  FILLER  REDEFINES   VAT1-LEDTEXTER.                                 
026610    05   VAT1-LEDTEXT              PIC X(42)   OCCURS  8 TIMES.           
026611     SKIP2                                                                
026620  03  VAT2-LEDTEXTER.                                                     
026630    05 FILLER PIC X(42)                                                   
026640              VALUE 'ART. 39 BIS OF FREE DUTCH VAT-CODE        '.         
026650    05 FILLER PIC X(42)                                                   
026651              VALUE 'ART. 39 BIS OF FREE DUTCH VAT-CODE        '.         
026670    05 FILLER PIC X(42)                                                   
026671              VALUE 'ART. 39 BIS OF FREE DUTCH VAT-CODE        '.         
026690    05 FILLER PIC X(42)                                                   
026691              VALUE 'ART. 39 BIS OF FREE DUTCH VAT-CODE        '.         
026693    05 FILLER PIC X(42)                                                   
026694              VALUE 'ART. 39 BIS OF FREE DUTCH VAT-CODE        '.         
026696    05 FILLER PIC X(42)                                                   
026697              VALUE 'ART. 39 BIS OF FREE DUTCH VAT-CODE        '.         
026698    05 FILLER PIC X(42)                                                   
026699              VALUE 'ART. 39 BIS OF FREE DUTCH VAT-CODE        '.         
026700    05 FILLER PIC X(42)                                                   
026701              VALUE 'ART. 39 BIS OF FREE DUTCH VAT-CODE        '.         
026702*                                                                         
026703  03  FILLER  REDEFINES   VAT2-LEDTEXTER.                                 
026704    05   VAT2-LEDTEXT              PIC X(42)   OCCURS  8 TIMES.           
026710     SKIP2                                                                
026720*            ***  LEVERANSLAGER LEDTEXTER DC ***                          
026730*                                                                         
026801*VVVVVVVVVVV                                                              
026810  03  LEVORT-DC-LEDTEXTER.                                                
026820    05 LEVORT-LEDTEXT-CDC-SE.                                             
026830      07 FILLER                    PIC X(33) VALUE                        
026840         'DELIVERED FROM CDC, GOTHENBURG   '.                             
026841                                                                          
026850    05 LEVORT-LEDTEXT-SDC-NL.                                             
026860      07 FILLER                    PIC X(33) VALUE                        
026870         'DELIVERED FROM SDC, MAASTRICHT   '.                             
026880                                                                          
026881    05 LEVORT-LEDTEXT-SDC-GB.                                             
026882      07 FILLER                    PIC X(33) VALUE                        
026883         'DELIVERED FROM SDC, CRICK        '.                             
026884                                                                          
026881    05 LEVORT-LEDTEXT-LDC-GB-2A.                                          
026882      07 FILLER                    PIC X(33) VALUE                        
026883         'DELIVERED FROM LDC, CAMBRIDGE    '.                             
026884                                                                          
026885    05 LEVORT-LEDTEXT-SDC-ES.                                             
026886      07 FILLER                    PIC X(33) VALUE                        
026887         'ENTREGADO POR EL SDC, AZUQUECA   '.                             
026889                                                                          
026890    05 LEVORT-LEDTEXT-SDC-IT.                                             
026891      07 FILLER                    PIC X(33) VALUE                        
026892         '                                 '.                             
026894                                                                          
026895    05 LEVORT-LEDTEXT-SDC-AT.                                             
026896      07 FILLER                    PIC X(33) VALUE                        
026898         'DELIVERED FROM SDC, VIENNA       '.                             
026899                                                                          
026900    05 LEVORT-LEDTEXT-NDC-JP.                                             
026901      07 FILLER                    PIC X(33) VALUE                        
026903         'DELIVERED FROM NDC, NAGOYA       '.                             
026904                                                                          
026905    05 LEVORT-LEDTEXT-NDC-AU.                                             
026906      07 FILLER                    PIC X(33) VALUE                        
026907         'DELIVERED FROM NDC, MINTO        '.                             
026908                                                                          
026909    05 LEVORT-LEDTEXT-DDC-SE.                                             
026910      07 FILLER                    PIC X(33) VALUE                        
026911         'DIRECT DELIVERY FROM SWEDEN      '.                             
026912                                                                          
026913    05 LEVORT-LEDTEXT-DDC-NO.                                             
026914      07 FILLER                    PIC X(33) VALUE                        
026915         'DIRECT DELIVERY FROM NORWAY      '.                             
026916                                                                          
026917    05 LEVORT-LEDTEXT-DDC-BE.                                             
026918      07 FILLER                    PIC X(33) VALUE                        
026919         'DIRECT DELIVERY FROM BELGIUM     '.                             
026920                                                                          
026917    05 LEVORT-LEDTEXT-DDC-DE.                                             
026918      07 FILLER                    PIC X(33) VALUE                        
026919         'DIRECT DELIVERY FROM GERMANY     '.                             
026920                                                                          
026921                                                                          
026922*            ***  LEGAL AGENT  LEDTEXTER  ***                             
026923*                                                                         
026924  03  LEGAL-AGENT-LEDTEXTER-SDC21.                                        
026925    04 LEGAL-RUBRIK-SDC21.                                                
026926      05 FILLER PIC X(90) VALUE                                           
026927        'LEGAL AGENT FOR VOLVO CARS IN THE NETHERLANDS:'.                 
026929*                                                                         
026930    04 LEGAL-NAME-SDC21.                                                  
026931      05 FILLER PIC X(90) VALUE                                           
026932        'NAME: VOLVO CARS NETHERLAND B.V.'.                               
026934*                                                                         
026935    04 LEGAL-ADRESS-1-SDC21.                                              
026936      05 FILLER PIC X(90) VALUE                                           
026937        'ADR:  GRAANMOLENWEG 9-11'.                                       
026939*                                                                         
026940    04 LEGAL-ADRESS-2-SDC21.                                              
026941      05 FILLER PIC X(90) VALUE                                           
026942        '      6229 PA MAASTRICHT'.                                       
026944*                                                                         
026945    04 LEGAL-ADRESS-3-SDC21.                                              
026946      05 FILLER PIC X(90) VALUE                                           
026947        '      THE NETHERLANDS'.                                          
026949*                                                                         
026950*                                                                         
026951  03  LEGAL-AGENT-LEDTEXTER-SDC22.                                        
026952    04 LEGAL-RAD1-LEDT-SDC22.                                             
026953      05 FILLER PIC X(21) VALUE                                           
026954        'REGLEMENT AU         '.                                          
026955*                                                                         
026956    04 LEGAL-RAD2-LEDT-SDC22.                                             
026957      05 FILLER PIC X(40) VALUE                                           
026958        'PAIEMENT ANTICIPE   : REMISE 0%        '.                        
026959*                                                                         
026960    04 LEGAL-RAD3-LEDT-SDC22.                                             
026961      05 FILLER PIC X(40) VALUE                                           
026962        'INTERET POUR RETARD : TAUX VOLVO ACTUEL'.                        
026963*                                                                         
026964*                                                                         
026965  03  LEGAL-AGENT-LEDTEXTER-SDC25.                                        
026966    04 LEGAL-RUBRIK-SDC25.                                                
026967      05 FILLER PIC X(90) VALUE                                           
026970        'RAPRESENTANTE LEGALE    '.                                       
026990*                                                                         
026991    04 LEGAL-NAME-SDC25.                                                  
026992      05 FILLER PIC X(90) VALUE                                           
026993        'NOME: VOLVO ITALIA  SPA '.                                       
026995*                                                                         
026996    04 LEGAL-ADRESS-1-SDC25.                                              
026997      05 FILLER PIC X(90) VALUE                                           
026998        'ADR:  VIA E. MATTE 66   '.                                       
027000*                                                                         
027010    04 LEGAL-ADRESS-2-SDC25.                                              
027020      05 FILLER PIC X(90) VALUE                                           
027030        '     40138 BOLOGNA      '.                                       
027050*                                                                         
027060    04 LEGAL-ADRESS-3-SDC25.                                              
027070      05 FILLER PIC X(90) VALUE                                           
027080        '      ITALY             '.                                       
027091*                                                                         
027093  03  IDBIL-LEDTEXTER.                                                    
027094    05 FILLER PIC X(22) VALUE '      BIL IDENTITET  :'.                   
027096    05 FILLER PIC X(22) VALUE '       CAR IDENTITY  :'.                   
027097    05 FILLER PIC X(22) VALUE 'IDENTITÉ DE VOITURE  :'.                   
027098    05 FILLER PIC X(22) VALUE 'AUTOMOVIL IDENTIDAD  :'.                   
027100    05 FILLER PIC X(22) VALUE '     AUTO IDENTITÄT  :'.                   
027102    05 FILLER PIC X(14) VALUE '      IDENTITA'.                           
027103    05 FILLER PIC X(01) VALUE QUOTE.                                      
027104    05 FILLER PIC X(7)  VALUE ' AUTO :'.                                  
027106    05 FILLER PIC X(22) VALUE '          AUTO TYPE  :'.                   
027107    05 FILLER PIC X(22) VALUE '       CAR IDENTITY  :'.                   
027109*                                                                         
027110  03  FILLER  REDEFINES   IDBIL-LEDTEXTER.                                
027111    05   IDBIL-LEDTEXT             PIC X(22)   OCCURS  8 TIMES.           
027112     SKIP2                                                                
027114*                                                                         
027120*                                                                         
027200*** END COPY W475W551    LENGTH=                                          
