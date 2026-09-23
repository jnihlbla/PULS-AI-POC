000010*** EDIT ALLOWED                                                          
000100 01  W475W554.                                                            
000200*                                                                         
000300*                                                                         
000400*       LEDTEXTER FÖR UTSKRIFT AV TOTALER I 8 MÖJLIGA SPRÅK               
000500*                                                                         
000600*        1 - SVENSKA                                                      
000700*        2 - ENGELSKA                                                     
000800*        3 - FRANSKA                                                      
000900*        4 - SPANSKA                                                      
001000*        5 - TYSKA                                                        
001100*        7 - ITALIENSKA                                                   
001200*        6 - FLAMLÄNDSKA                                                  
001300*        8 - LEDIGT                                                       
001400*                                                                         
001500*                                                                         
001600*                      ***  VARUVÄRDE  LEDTEXTER                          
001700*                                                                         
001800   03  BEVAVARDE-LEDTEXTER.                                               
001900*                                                                         
002000      07  FILLER         PIC X(16)    VALUE 'VARUVÄRDE       '.           
002100      07  FILLER         PIC X(16)    VALUE 'GOODS VALUE     '.           
002200      07  FILLER         PIC X(16)    VALUE 'VAL.MARCHANDISES'.           
002300      07  FILLER         PIC X(16)    VALUE 'VALOR           '.           
002400      07  FILLER         PIC X(16)    VALUE 'WERT            '.           
002500      07  FILLER         PIC X(16)    VALUE 'VALORE MERCE    '.           
002600      07  FILLER         PIC X(16)    VALUE 'WAARDE          '.           
002700      07  FILLER         PIC X(16)    VALUE 'GOODS VALUE     '.           
002800*                                                                         
002900   03  FILLER  REDEFINES  BEVAVARDE-LEDTEXTER.                            
003000     05  BEVAVARDE-LEDTEXT       PIC X(16)   OCCURS  8 TIMES.             
003100     SKIP1                                                                
003200*                      ***  SUMMA  LEDTEXTER                              
003300*                                                                         
003400   03  SUFKTBEL-LEDTEXTER.                                                
003500*                                                                         
003600      07  FILLER         PIC X(16)    VALUE 'FAKTURERAT VÄRDE'.           
003700      07  FILLER         PIC X(16)    VALUE 'TOTAL VALUE     '.           
003800      07  FILLER         PIC X(16)    VALUE 'VALEUR TOTALE TC'.           
003900      07  FILLER         PIC X(16)    VALUE 'VALOR TOTAL     '.           
004000      07  FILLER         PIC X(16)    VALUE 'WERT TOTAL      '.           
004100      07  FILLER         PIC X(16)    VALUE 'VALORE TOTALE   '.           
004200      07  FILLER         PIC X(16)    VALUE 'WAARDE TOTAL    '.           
004300      07  FILLER         PIC X(16)    VALUE 'TOTAL VALUE     '.           
004400*                                                                         
004500   03  FILLER  REDEFINES  SUFKTBEL-LEDTEXTER.                             
004600     05  SUFKTBEL-LEDTEXT      PIC X(16)   OCCURS  8 TIMES.               
004700     SKIP3                                                                
004800*                      ***  LIKAMED  LEDTEXTER                            
004900*                                                                         
005000   03  BELIKAMED-LEDTEXTER.                                               
005100*                                                                         
005200      07  FILLER             PIC X(08)    VALUE 'LIKA MED'.               
005300      07  FILLER             PIC X(08)    VALUE 'EQUAL TO'.               
005400      07  FILLER             PIC X(08)    VALUE 'EGAL A  '.               
005500      07  FILLER             PIC X(08)    VALUE 'IGUAL A '.               
005600      07  FILLER             PIC X(08)    VALUE 'GLEICH  '.               
005700      07  FILLER             PIC X(08)    VALUE 'UQUALE A'.               
005800      07  FILLER             PIC X(08)    VALUE 'GELYK AN'.               
005900      07  FILLER             PIC X(08)    VALUE 'EQUAL TO'.               
006000*                                                                         
006100   03  FILLER  REDEFINES  BELIKAMED-LEDTEXTER.                            
006200     05  BELIKAMED-LEDTEXT       PIC X(08)   OCCURS  8 TIMES.             
006300     SKIP1                                                                
006400*                   ***   PRKURS  LEDTEXTER                               
006500*                                                                         
006600   03  PRKURS-LEDTEXTER.                                                  
006700*                                                                         
006800      07  FILLER             PIC X(06)    VALUE 'KURS  '.                 
006900      07  FILLER             PIC X(06)    VALUE 'RATE  '.                 
007000      07  FILLER             PIC X(06)    VALUE 'COURS '.                 
007100      07  FILLER             PIC X(06)    VALUE 'TASA  '.                 
007200      07  FILLER             PIC X(06)    VALUE 'KURS  '.                 
007300      07  FILLER             PIC X(06)    VALUE 'CAMBIO'.                 
007400      07  FILLER             PIC X(06)    VALUE 'KOERS '.                 
007500      07  FILLER             PIC X(06)    VALUE 'RATE  '.                 
007600*                                                                         
007700   03  FILLER  REDEFINES  PRKURS-LEDTEXTER.                               
007800     05  PRKURS-LEDTEXT       PIC X(06)   OCCURS  8 TIMES.                
007900*                                                                         
008000*                                                                         
008100*                                                                         
008200 01  AVSLUT-LEDTEXTER.                                                    
008300   03  AVSLUT-LEDTEXT-SVENSK.                                             
008400     05  FILLER  PIC X(30) VALUE 'EMBALLAGE OCH HANTERING       '.        
008500     05  FILLER  PIC X(30) VALUE 'TILLÄGG FÖR DAGORDER          '.        
008600     05  FILLER  PIC X(30) VALUE 'FRAKT                         '.        
008700     05  FILLER  PIC X(30) VALUE 'FÖRSÄKRING                    '.        
008800     05  FILLER  PIC X(30) VALUE 'LEGALISERING                  '.        
008900     05  FILLER  PIC X(10) VALUE 'RÄNTESATS:'.                            
009000     05  FILLER  PIC X(15) VALUE '  RÄNTEKOSTNAD:'.                       
009100     05  FILLER  PIC X(30) VALUE 'AVDRAG                        '.        
009200     05  FILLER  PIC X(15) VALUE 'EMBALLAGE      '.                       
009300     05  FILLER  PIC X(15) VALUE 'HANTERING      '.                       
009400     SKIP3                                                                
009500   03  AVSLUT-LEDTEXT-ENGELSK.                                            
009600     05  FILLER  PIC X(30) VALUE 'PACKING AND HANDLING COST     '.        
009700     05  FILLER  PIC X(30) VALUE 'DAILY ORDER SURCHARGE         '.        
009800     05  FILLER  PIC X(30) VALUE 'FREIGHT COST                  '.        
009900     05  FILLER  PIC X(30) VALUE 'INSURANCE COST                '.        
010000     05  FILLER  PIC X(30) VALUE 'LEGALISATION COST             '.        
010100     05  FILLER  PIC X(10) VALUE 'INTEREST :'.                            
010200     05  FILLER  PIC X(15) VALUE ' INTEREST COST:'.                       
010300     05  FILLER  PIC X(30) VALUE 'DEDUCTION                     '.        
010400     05  FILLER  PIC X(15) VALUE 'PACKING        '.                       
010500     05  FILLER  PIC X(15) VALUE 'FREIGHT        '.                       
010600     SKIP3                                                                
010700   03  AVSLUT-LEDTEXT-FRANSKT.                                            
010800     05  FILLER  PIC X(30) VALUE 'EMBALLAGE ET MANUTENTION      '.        
010900     05  FILLER  PIC X(30) VALUE 'DAILY ORDER SURCHARGE         '.        
011000     05  FILLER  PIC X(30) VALUE 'FRAIS DE TRANSPORT            '.        
011100     05  FILLER  PIC X(30) VALUE 'ASSURANCE                     '.        
011200     05  FILLER  PIC X(30) VALUE 'FRAIS DE LEGALISATION         '.        
011300     05  FILLER  PIC X(10) VALUE 'TAUX INT.:'.                            
011400     05  FILLER  PIC X(15) VALUE 'FRAIS INTERETS:'.                       
011500     05  FILLER  PIC X(30) VALUE 'DEDUCTIONS                    '.        
011600     05  FILLER  PIC X(15) VALUE 'EMBALLAGE      '.                       
011700     05  FILLER  PIC X(15) VALUE 'MANUTENTION    '.                       
011800     SKIP3                                                                
011900   03  AVSLUT-LEDTEXT-SPANSK.                                             
012000     05  FILLER  PIC X(30) VALUE 'GASTOS DE EMBALAJE            '.        
012100     05  FILLER  PIC X(30) VALUE 'DAILY ORDER SURCHARGE         '.        
012200     05  FILLER  PIC X(30) VALUE 'COSTO DE FLETE                '.        
012300     05  FILLER  PIC X(30) VALUE 'PRIMA DE SEGURO               '.        
012400     05  FILLER  PIC X(30) VALUE 'GASTOS DE LEGALIZACION        '.        
012500     05  FILLER  PIC X(10) VALUE 'TASA INT.:'.                            
012600     05  FILLER  PIC X(15) VALUE 'GASTOS INTERES:'.                       
012700     05  FILLER  PIC X(30) VALUE 'DEDUCCION                     '.        
012800     05  FILLER  PIC X(15) VALUE 'GASTOS         '.                       
012900     05  FILLER  PIC X(15) VALUE 'EMBALAGE       '.                       
013000     SKIP3                                                                
013100   03  AVSLUT-LEDTEXT-TYSK.                                               
013200     05  FILLER  PIC X(30) VALUE 'P & H                         '.        
013300     05  FILLER  PIC X(30) VALUE 'DAILY ORDER SURCHARGE         '.        
013400     05  FILLER  PIC X(30) VALUE 'FRACHT KOSTEN                 '.        
013500     05  FILLER  PIC X(30) VALUE 'VERSICHERUNGSKOSTEN           '.        
013600     05  FILLER  PIC X(30) VALUE 'LEGALISIERUNGSKOSTEN          '.        
013700     05  FILLER  PIC X(10) VALUE 'ZINS     :'.                            
013800     05  FILLER  PIC X(15) VALUE '    ZINSKOSTEN:'.                       
013900     05  FILLER  PIC X(30) VALUE 'RABATT                        '.        
014000     05  FILLER  PIC X(15) VALUE 'VERPACKUNG     '.                       
014100     05  FILLER  PIC X(15) VALUE 'HANTERIERUNGSK '.                       
014200     SKIP3                                                                
014300   03  AVSLUT-LEDTEXT-ITALIEN.                                            
014400     05  FILLER  PIC X(30) VALUE 'COSTO IMBALLO E SPEDIZIONE    '.        
014500     05  FILLER  PIC X(30) VALUE 'SOVRAPREZZ. ORD. URGENTE      '.        
014600     05  FILLER  PIC X(30) VALUE 'COSTO TRANSPORTO              '.        
014700     05  FILLER  PIC X(30) VALUE 'COSTO ASSICURAZIONE           '.        
014800     05  FILLER  PIC X(30) VALUE 'COSTO IMPORTAZIONE            '.        
014900     05  FILLER  PIC X(10) VALUE 'INTERESSE:'.                            
015000     05  FILLER  PIC X(15) VALUE 'COSTO PER INT.:'.                       
015100     05  FILLER  PIC X(30) VALUE 'DEDUZIONE                     '.        
015200     05  FILLER  PIC X(15) VALUE 'IMBALLO        '.                       
015300     05  FILLER  PIC X(15) VALUE 'TRANSPORTO     '.                       
015400     SKIP3                                                                
015500   03  AVSLUT-LEDTEXT-NDRLND.                                             
015600     05  FILLER  PIC X(30) VALUE 'HANTERINGSKOSTEN              '.        
015700     05  FILLER  PIC X(30) VALUE 'DAILY ORDER SURCHARGE         '.        
015800     05  FILLER  PIC X(30) VALUE 'VRACHT KOSTEN                 '.        
015900     05  FILLER  PIC X(30) VALUE 'VERZEKERINGSKOSTEN            '.        
016000     05  FILLER  PIC X(30) VALUE 'LEGALISATIE KOSTEN            '.        
016100     05  FILLER  PIC X(10) VALUE 'INT.VOET :'.                            
016200     05  FILLER  PIC X(15) VALUE 'INTREST KOSTEN:'.                       
016300     05  FILLER  PIC X(30) VALUE 'RABATT                        '.        
016400     05  FILLER  PIC X(15) VALUE 'HANTERINGSK '.                          
016500     05  FILLER  PIC X(15) VALUE '               '.                       
016600     SKIP3                                                                
016700   03  AVSLUT-LEDTEXT-LEDIGT.                                             
016800     05  FILLER  PIC X(30) VALUE 'PACKING AND HANDLING COST     '.        
016900     05  FILLER  PIC X(30) VALUE 'DAILY ORDER SURCHARGE         '.        
017000     05  FILLER  PIC X(30) VALUE 'FREIGHT COST                  '.        
017100     05  FILLER  PIC X(30) VALUE 'INSURANCE COST                '.        
017200     05  FILLER  PIC X(30) VALUE 'LEGALISATION COST             '.        
017300     05  FILLER  PIC X(10) VALUE 'INTEREST :'.                            
017400     05  FILLER  PIC X(15) VALUE ' INTEREST COST:'.                       
017500     05  FILLER  PIC X(30) VALUE 'DEDUCTION                     '.        
017600     05  FILLER  PIC X(15) VALUE 'PACKING        '.                       
017700     05  FILLER  PIC X(15) VALUE 'FREIGHT        '.                       
017800     SKIP3                                                                
017900 01  FILLER  REDEFINES AVSLUT-LEDTEXTER.                                  
018000   03  FILLER  OCCURS 8 TIMES.                                            
018100     05  AVSLUT-PREMBHNT-LEDTEXT  PIC X(30).                              
018200     05  AVSLUT-TILLAGG-LEDTEXT   PIC X(30).                              
018300     05  AVSLUT-PRFRAKT-LEDTEXT   PIC X(30).                              
018400     05  AVSLUT-PRFOERS-LEDTEXT   PIC X(30).                              
018500     05  AVSLUT-PRLEGKST-LEDTEXT  PIC X(30).                              
018600     05  AVSLUT-RERANTA-LEDTEXT   PIC X(10).                              
018700     05  AVSLUT-PRRANTA-LEDTEXT   PIC X(15).                              
018800     05  AVSLUT-PRAVDRAG-LEDTEXT  PIC X(30).                              
018900     05  AVSLUT-PACKING-LEDTEXT   PIC X(15).                              
019000     05  AVSLUT-HANDLING-LEDTEXT  PIC X(15).                              
019100     EJECT                                                                
019200 01  SDC-SPECIAL-LEDTEXTER.                                               
019300*                                                                         
019400   03  SDC-BEVAVARDE-LEDTEXTER.                                           
019500    05 FILLER  PIC X(30) VALUE 'VARUVÄRDE SVENSKA KRONOR      '.          
019600    05 FILLER  PIC X(30) VALUE 'GOODS VALUE IN SWEDISH CROWNS '.          
019700    05 FILLER  PIC X(30) VALUE 'VALEUR EN COURONNES SUEDOISES '.          
019800    05 FILLER  PIC X(30) VALUE 'VALOR CORONAS SUECAS          '.          
019900    05 FILLER  PIC X(30) VALUE 'WERT   IN SEK                 '.          
020100    05 FILLER  PIC X(30) VALUE 'VAL.DELLA MERCE IN CORONE SVE.'.          
020110    05 FILLER  PIC X(30) VALUE 'WAARDE IN SEK                 '.          
020200    05 FILLER  PIC X(30) VALUE 'GOODS VALUE IN SWEDISH CROWNS '.          
020300   03  FILLER  REDEFINES  SDC-BEVAVARDE-LEDTEXTER.                        
020400     05  SDC-BEVAVARDE-LEDTEXT      PIC X(30)   OCCURS  8 TIMES.          
020500*                                                                         
020600   03  SDC-BASBELOPP-LEDTEXTER.                                           
020700    05 FILLER  PIC X(12) VALUE '   BASBELOPP'.                            
020800    05 FILLER  PIC X(12) VALUE '  BASE VALUE'.                            
020900    05 FILLER  PIC X(12) VALUE ' BASE CALCUL'.                            
021000    05 FILLER  PIC X(12) VALUE '    SUMA BAS'.                            
021100    05 FILLER  PIC X(12) VALUE '        WERT'.                            
021300    05 FILLER  PIC X(12) VALUE '  IMPONIBILE'.                            
021310    05 FILLER  PIC X(12) VALUE '      WAARDE'.                            
021400    05 FILLER  PIC X(12) VALUE '  BASE VALUE'.                            
021500   03  FILLER  REDEFINES  SDC-BASBELOPP-LEDTEXTER.                        
021600     05  SDC-BASBELOPP-LEDTEXT PIC X(12) OCCURS  8 TIMES.                 
021700*                                                                         
021800   03  SDC-KDTVA-LEDTEXTER.                                               
021900    05 FILLER  PIC X(04) VALUE 'VAT '.                                    
022000    05 FILLER  PIC X(04) VALUE 'VAT '.                                    
022100    05 FILLER  PIC X(04) VALUE 'TVA '.                                    
022200    05 FILLER  PIC X(04) VALUE 'IMP.'.                                    
022300    05 FILLER  PIC X(04) VALUE 'MWST'.                                    
022400    05 FILLER  PIC X(04) VALUE 'IVA '.                                    
022500    05 FILLER  PIC X(04) VALUE 'VAT '.                                    
022600    05 FILLER  PIC X(04) VALUE 'VAT '.                                    
022700   03  FILLER  REDEFINES  SDC-KDTVA-LEDTEXTER.                            
022800     05  SDC-KDTVA-LEDTEXT    PIC X(04) OCCURS  8 TIMES.                  
022900*                                                                         
023000   03  SDC-RETVA-LEDTEXTER.                                               
023100    05 FILLER  PIC X(04) VALUE '   %'.                                    
023200    05 FILLER  PIC X(04) VALUE 'RATE'.                                    
023300    05 FILLER  PIC X(04) VALUE '   %'.                                    
023400    05 FILLER  PIC X(04) VALUE 'TASA'.                                    
023500    05 FILLER  PIC X(04) VALUE '   %'.                                    
023700    05 FILLER  PIC X(04) VALUE '   %'.                                    
023710    05 FILLER  PIC X(04) VALUE '   %'.                                    
023800    05 FILLER  PIC X(04) VALUE 'RATE'.                                    
023900   03  FILLER  REDEFINES  SDC-RETVA-LEDTEXTER.                            
024000     05  SDC-RETVA-LEDTEXT    PIC X(04) OCCURS  8 TIMES.                  
024100*                                                                         
024200   03  SDC-PRMOMBEC-LEDTEXTER.                                            
024300    05 FILLER  PIC X(12) VALUE '  BELOPP VAT'.                            
024400    05 FILLER  PIC X(12) VALUE '  AMOUNT VAT'.                            
024500    05 FILLER  PIC X(12) VALUE ' MONTANT TVA'.                            
024600    05 FILLER  PIC X(12) VALUE '    SUMA IMP'.                            
024700    05 FILLER  PIC X(12) VALUE '      BETRAG'.                            
024800    05 FILLER  PIC X(12) VALUE ' IMPORTO IVA'.                            
024900    05 FILLER  PIC X(12) VALUE '  AMOUNT VAT'.                            
025000    05 FILLER  PIC X(12) VALUE '  AMOUNT VAT'.                            
025100   03  FILLER  REDEFINES  SDC-PRMOMBEC-LEDTEXTER.                         
025200     05  SDC-PRMOMBEC-LEDTEXT PIC X(12) OCCURS  8 TIMES.                  
025300*                                                                         
025400   03  SDC-SUVAT-LEDTEXTER.                                               
025500    05 FILLER  PIC X(20) VALUE 'SUMMA VAT           '.                    
025600    05 FILLER  PIC X(20) VALUE 'TOTAL VAT           '.                    
025700    05 FILLER  PIC X(20) VALUE 'TOTAL T.V.A.        '.                    
025800    05 FILLER  PIC X(20) VALUE 'TOTAL IMPUESTO      '.                    
025900    05 FILLER  PIC X(20) VALUE 'TOTAL MWST          '.                    
026000    05 FILLER  PIC X(20) VALUE 'TOTALE IVA          '.                    
026100    05 FILLER  PIC X(20) VALUE 'TOTAL VAT IN NLG    '.                    
026200    05 FILLER  PIC X(20) VALUE 'TOTAL VAT IN NLG    '.                    
026300   03  FILLER  REDEFINES  SDC-SUVAT-LEDTEXTER.                            
026400     05  SDC-SUVAT-LEDTEXT    PIC X(20) OCCURS  8 TIMES.                  
026500*                                                                         
026600   03  SDC-SUFKTLOC-VAT-LEDTEXTER.                                        
026700    05 FILLER  PIC X(20) VALUE 'TOTAL INKLUSIVE VAT '.                    
026800    05 FILLER  PIC X(20) VALUE 'TOTAL VAT INCLUDED  '.                    
026900    05 FILLER  PIC X(20) VALUE 'TOTAL TVA COMPRISE  '.                    
027000    05 FILLER  PIC X(20) VALUE 'TOTAL CON IMPUESTO  '.                    
027100    05 FILLER  PIC X(20) VALUE 'TOTAL INKL MWST     '.                    
027200    05 FILLER  PIC X(20) VALUE 'TOTALE IVA INCLUSA  '.                    
027300    05 FILLER  PIC X(20) VALUE 'TOTAL VAT INCLUDED  '.                    
027400    05 FILLER  PIC X(20) VALUE 'TOTAL VAT INCLUDED  '.                    
027500   03  FILLER  REDEFINES  SDC-SUFKTLOC-VAT-LEDTEXTER.                     
027600     05  SDC-SUFKTLOC-VAT-LEDTEXT PIC X(20) OCCURS  8 TIMES.              
027700*                                                                         
027800   03  SDC-BEBETVAL-LEDTEXTER.                                            
027900    05 FILLER  PIC X(20) VALUE 'BETALNINGSVALUTA    '.                    
028000    05 FILLER  PIC X(20) VALUE 'INVOICE PAYABLE IN  '.                    
028100    05 FILLER  PIC X(20) VALUE 'FACTURE PAYABLE EN  '.                    
028200    05 FILLER  PIC X(20) VALUE 'FACTURA PAGADERO EN '.                    
028300    05 FILLER  PIC X(20) VALUE 'RECHNUNG ZAHLBAR IN '.                    
028510    05 FILLER  PIC X(20) VALUE 'FATTURA PAGABILE IN '.                    
028520    05 FILLER  PIC X(20) VALUE 'INVOICE PAYABLE IN  '.                    
028530    05 FILLER  PIC X(20) VALUE 'INVOICE PAYABLE IN  '.                    
028700   03  FILLER  REDEFINES  SDC-BEBETVAL-LEDTEXTER.                         
028800     05  SDC-BEBETVAL-LEDTEXT      PIC X(20)   OCCURS  8 TIMES.           
028900*                                                                         
029000*L1  W475W554            PIC X(276).                                      
029100*L1  AVSLUT-LEDTEXTER    PIC X(1410).                                     
029200*L1  FILLER  REDEFINES AVSLUT-LEDTEXTER PIC X(1410).                      
029300*L1  SDC-SPECIAL-LEDTEXTER PIC X(732).                                    
029400*** END COPY W475W554    LENGTH=      OLD LENGTH=                         
