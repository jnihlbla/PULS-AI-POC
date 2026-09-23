000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4069700.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   95/09/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SKAPAR PRELIMINÄR OCH TOTALLISTA FÖR ITALIEN.                    
000900*        BOLLA.                                                           
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300*        PROGRAMMET LÄSER      WL4491 (WDR4)                              
001400*                              WLGMTA (WDB2)                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T697                                              
001800*        MID:         W4I69701                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O69701                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W4069700'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                         PIC X          VALUE 'J'.                 
003600 77  NEJ                        PIC X          VALUE 'N'.                 
003700 77  INDX                       PIC S9(9)     VALUE +0 COMP SYNC.         
003800 77  MAX-INDX                   PIC S9(9)     VALUE +10 COMP SYNC.        
003900 77  SPAR-IDZON                 PIC X(2)       VALUE SPACE.               
004000 77  SPAR-IDKUNDNR              PIC S9(7)      VALUE ZERO COMP-3.         
004100 77  WS-IDKUNDNR                PIC S9(7)      VALUE ZERO COMP-3.         
004200                                                                          
004300 77  WS-IDTRPTNR                PIC S9(3)      VALUE ZERO COMP-3.         
004400 77  WS-KDFRAKT                 PIC S9(3)      VALUE ZERO COMP-3.         
004500                                                                          
004600 77  WS-IDTRPBOR                PIC S9(5)      VALUE ZERO COMP-3.         
004700 77  WS-ANTAL-KUND              PIC S9(3)      VALUE ZERO COMP-3.         
004800 77  WS-ANTAL-BOLLA             PIC S9(3)      VALUE ZERO COMP-3.         
004900 77  WS-ANTAL-ORDER             PIC S9(3)      VALUE ZERO COMP-3.         
005000 77  WS-RAD-RAKNARE1            PIC S9(3)      VALUE ZERO COMP-3.         
005100 77  WS-SID-RAKNARE1            PIC S9(3)      VALUE ZERO COMP-3.         
005200 77  WS-RAD-RAKNARE2            PIC S9(3)      VALUE ZERO COMP-3.         
005300 77  WS-SID-RAKNARE2            PIC S9(3)      VALUE ZERO COMP-3.         
005400 77  WS-RAD-RAKNARE3            PIC S9(3)      VALUE ZERO COMP-3.         
005500 77  WS-SID-RAKNARE3            PIC S9(3)      VALUE ZERO COMP-3.         
005600********** ÄNDRA PRINTER ********                                         
005700 77  WS-PRT1                    PIC X(8)       VALUE 'AZ8     '.          
005800 77  WS-PRT2                    PIC X(8)       VALUE '4SUSA   '.          
005900 77  DUMMY-AREA                 PIC X(50)      VALUE SPACE.               
006000*********************************                                         
006100 77  WS-SUM-KUND-KOLLI          PIC S9(5)      VALUE ZERO COMP-3.         
006200 77  WS-SUM-KUND-VIKT           PIC S9(6)V9(1) VALUE ZERO COMP-3.         
006300 77  WS-SUM-KUND-VOLYM          PIC S9(4)V9(3) VALUE ZERO COMP-3.         
006400 77  WS-SUM-TOT-KOLLI           PIC S9(5)      VALUE ZERO COMP-3.         
006500 77  WS-SUM-TOT-VIKT            PIC S9(6)V9(1) VALUE ZERO COMP-3.         
006600 77  WS-SUM-TOT-VOLYM           PIC S9(4)V9(3) VALUE ZERO COMP-3.         
006700                                                                          
006800                                                                          
006900 01  WS-BOLLANR                  PIC X(8)      VALUE SPACE.               
007000                                                                          
007100 01  FILLER REDEFINES WS-BOLLANR.                                         
007200     03  WS-IDTRPBOT             PIC X.                                   
007300     03  WS-IDTRPBON             PIC 9(7).                                
007400                                                                          
007500 01  VCOM-DATUM                  PIC 9(6).                                
007600                                                                          
007700 01  DAGENS-DATUM.                                                        
007800     03  DAGENS-AAR              PIC 9(2).                                
007900     03  DAGENS-MAN              PIC 9(2).                                
008000     03  DAGENS-DAG              PIC 9(2).                                
008100                                                                          
008200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008300                                                                          
008400                                                                          
008500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008600     88  NYCKLAR-OK                          VALUE 'J'.                   
008700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008800                                                                          
008900 77  KUND-SW                     PIC X       VALUE 'J'.                   
009000     88  NY-KUND                             VALUE 'J'.                   
009100     88  GAMMAL-KUND                         VALUE 'N'.                   
009200                                                                          
009300 77  ZONA-SW                     PIC X       VALUE 'J'.                   
009400     88  NY-ZONA                             VALUE 'J'.                   
009500     88  GAMMAL-ZONA                         VALUE 'N'.                   
009600                                                                          
009700 77  BOLLADOK-SW                 PIC X       VALUE 'J'.                   
009800     88  BOLLADOK-REFNR-NY                   VALUE 'J'.                   
009900     88  BOLLADOK-REFNR-OLD                  VALUE 'N'.                   
010000                                                                          
010100 77  VCOM-SW                     PIC X       VALUE 'J'.                   
010200     88  VCOM-SKRIVS                         VALUE 'J'.                   
010300     88  VCOM-SKRIVS-EJ                      VALUE 'N'.                   
010400                                                                          
010500 77  TRAFF-SW                    PIC X       VALUE 'J'.                   
010600     88  TRAFF                               VALUE 'J'.                   
010700     88  TRAFF-EJ                            VALUE 'N'.                   
010800                                                                          
010900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011000     88  EGEN-MID                            VALUE '4697'.                
011100     88  GODK-MID                            VALUE '4691' '4692'          
011200                                                   '4693' '4694'          
011300                                                   '4695' '4696'          
011400                                                   '4697' '4698'          
011500                                                   '4699'.                
011600     88  HELP-MID                            VALUE '0551'.                
011700     EJECT                                                                
011800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011900 01  GENERELLA-SUBPROGRAM.                                                
012000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012400     03  W006PRC1                PIC X(8)    VALUE 'W006PRC1'.            
012500     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012800*01 -COPY WMEDAREA                                                        
012900     SKIP3                                                                
013000 01  MESSAGE-CODES.                                                       
013100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'W006PRAR'.            
013400                                                                          
013500*01  -COPY W006PRAR                                                       
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'W006PRVC'.            
013800                                                                          
013900*01  -COPY W006PRVC                                                       
014000     EJECT                                                                
014100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014400     SKIP3                                                                
014500*01 -COPY WMSGINIT                                                        
014600     SKIP3                                                                
014700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014800*                                                                         
014900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015000     SKIP3                                                                
015100*01  MID -COPY W4I69701                                                   
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015400     SKIP3                                                                
015500*01  -COPY WMSGAREA                                                       
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015800     SKIP3                                                                
015900*01  -COPY WMFSAREA                                                       
016000     EJECT                                                                
016100********  LIST RUBRIKER OCH RADER   ******************************        
016200***                                                                       
016300 01  WS-LISTRAD.                                                          
016400     03  LIST-RAD              PIC X(132).                                
016500                                                                          
016600************ PRELIMINÄRA LISTAN " STAMPA PARZIALE "*************          
016700                                                                          
016800 01  LIST-RADER.                                                          
016900                                                                          
017000     03 L1-STAMPA-RUB1.                                                   
017100       05 FILLER                   PIC X(4)   VALUE SPACE.                
017200       05 L1-LIST-ID               PIC X(10).                             
017300       05 FILLER                   PIC X(6)   VALUE SPACE.                
017400       05 L1-STAMPA-RUB1-DEL1      PIC X(55)  VALUE                       
017500       'DISTINTA  RIEPILOGATIVA  DI  SPEDIZIONE  -  DM.29.11.78'.         
017600       05 L1-STAMPA-RUB1-DEL2      PIC X(18)  VALUE                       
017700       '  ART.4  2 COMMA  '.                                              
017800       05 L1-STAMPA-RUB1-DAT-DAG   PIC 9(2).                              
017900       05 FILLER                   PIC X(1)  VALUE '.'.                   
018000       05 L1-STAMPA-RUB1-DAT-MAN   PIC 9(2).                              
018100       05 FILLER                   PIC X(1)  VALUE '.'.                   
018200       05 L1-STAMPA-RUB1-DAT-AAR   PIC 9(2).                              
018300       05 FILLER                   PIC X(1)  VALUE '.'.                   
018400       05 L1-STAMPA-RUB1-SIDA      PIC X(8)   VALUE 'PAGINA '.            
018500       05 L1-STAMPA-RUB1-SIDNR     PIC Z(2)9.                             
018600                                                                          
018700     03 L1-STAMPA-RUB2.                                                   
018800       05 FILLER                   PIC X(51)  VALUE SPACE.                
018900       05 L1-RUB2-RUB              PIC X(15)  VALUE                       
019000             'STAMPA PARZIALE'.                                           
019100                                                                          
019200     03 L1-STAMPA-RUB3.                                                   
019300       05 FILLER                   PIC X(3)   VALUE SPACE.                
019400       05 L1-RUB3                  PIC X(13)  VALUE                       
019500              'SPEDIZIONIERE'.                                            
019600       05 FILLER                   PIC X(1) VALUE SPACE.                  
019700       05 L1-STAMPA-RUB3-KDFRAKT   PIC Z(2)9.                             
019800       05 FILLER                   PIC X(1) VALUE SPACE.                  
019900       05 L1-STAMPA-RUB3-TRPNAMN   PIC X(15).                             
020000       05 FILLER                   PIC X(3)   VALUE SPACE.                
020100       05 L1-STAMPA-RUB3-TRPADR1   PIC X(35).                             
020200       05 L1-STAMPA-RUB3-TRPADR2   PIC X(35).                             
020300       05 L1-RUB3-DEL2             PIC X(5)   VALUE                       
020400         'ZONA '.                                                         
020500       05 L1-STAMPA-RUB3-IDZON     PIC X(2).                              
020600                                                                          
020700     03 L1-STAMPA-RUB4.                                                   
020800       05 FILLER                   PIC X(3)   VALUE SPACE.                
020900       05 L1-RUB4                  PIC X(14)  VALUE                       
021000               'CONCESSIONARIO'.                                          
021100       05 FILLER                   PIC X(1)   VALUE SPACE.                
021200       05 L1-STAMPA-IDKUNDNR       PIC Z(6)9.                             
021300       05 FILLER                   PIC X(1)   VALUE SPACE.                
021400       05 L1-STAMPA-RUB4-BEGODSM   PIC X(55).                             
021500       05 L1-STAMPA-RUB4-ADGODSMK  PIC X(35).                             
021600       05 L1-STAMPA-RUB4-SPED      PIC X(7)  VALUE                        
021700          'N.SPED '.                                                      
021800       05 L1-RUB4-ANT-KUND         PIC Z(2)9.                             
021900                                                                          
022000     03 L1-STAMPA-RUB5.                                                   
022100       05 FILLER                   PIC X(5)   VALUE SPACE.                
022200       05 L1-STAMPA-RUB5-1         PIC X(26)  VALUE                       
022300              'ORDINE  - DATA -   N.COLLI'.                               
022400       05 FILLER                   PIC X(11)  VALUE SPACE.                
022500       05 L1-STAMPA-RUB5-2         PIC X(33)  VALUE                       
022600               'PESO        VOLUME          BOLLA'.                       
022700                                                                          
022800     03 L1-STAMPA-RADER.                                                  
022900       05 FILLER                   PIC X(6)   VALUE SPACE.                
023000       05 L1-STAMPA-IDORDNR        PIC Z(4)9.                             
023100       05 FILLER                   PIC X(3)   VALUE SPACE.                
023200       05 L1-STAMPA-TIORDREG       PIC 9(6).                              
023300       05 FILLER                   PIC X(6)   VALUE SPACE.                
023400       05 L1-STAMPA-KVKOLLI        PIC Z(4)9.                             
023500       05 FILLER                   PIC X(9)  VALUE SPACE.                 
023600       05 L1-STAMPA-VKORDBTO       PIC Z(3)9.9.                           
023700       05 FILLER                   PIC X(6)   VALUE SPACE.                
023800       05 L1-STAMPA-VLORDBTO       PIC Z(3)9.9(3).                        
023900       05 FILLER                   PIC X(7)   VALUE SPACE.                
024000       05 FILLER                   PIC X(1)   VALUE 'H'.                  
024100       05 L1-STAMPA-IDTRPBON       PIC 9(7).                              
024200       05 FILLER                   PIC X(6)  VALUE SPACE.                 
024300       05 L1-STAMPA-TEXT1          PIC X(12).                             
024400       05 FILLER                   PIC X(6)  VALUE SPACE.                 
024500       05 L1-STAMPA-TEXT2          PIC X(23).                             
024600                                                                          
024700     03 L1-STAMPA-TOTAL-KUND.                                             
024800       05 FILLER                   PIC X(3)   VALUE SPACE.                
024900       05 TOT-PREL-RUB             PIC X(23)  VALUE                       
025000               'TOTALE CONCESSIONARIO  '.                                 
025100       05 TOT-PREL-ANT-KOLLI       PIC Z(4)9.                             
025200       05 FILLER                   PIC X(9)  VALUE SPACE.                 
025300       05 TOT-PREL-VIKT            PIC Z(3)9.9.                           
025400       05 FILLER                   PIC X(6)   VALUE SPACE.                
025500       05 TOT-PREL-VOLYM           PIC Z(3)9.9(3).                        
025600                                                                          
025700     03 L1-HELA-STAMPA-RAD1.                                              
025800       05 FILLER                   PIC X(3)   VALUE SPACE.                
025900       05 TOT-STAMPA-PREL-RUB1     PIC X(23)  VALUE                       
026000               'TOTALE TRASPORTATORE   '.                                 
026100       05 TOT-STAMPA-PREL-KOLLI    PIC Z(4)9.                             
026200       05 FILLER                   PIC X(9)  VALUE SPACE.                 
026300       05 TOT-STAMPA-PREL-VIKT     PIC Z(3)9.9.                           
026400       05 FILLER                   PIC X(6)   VALUE SPACE.                
026500       05 TOT-STAMPA-PREL-VOLYM    PIC Z(3)9.9(3).                        
026600       05 FILLER                   PIC X(2)   VALUE SPACE.                
026700       05 TOT-STAMPA-PREL-RUB2     PIC X(45)  VALUE                       
026800         'DATA E ORA TRASPORTO  FIRMA PER ACCETTAZIONE '.                 
026900       05 TOT-STAMPA-PREL-DEL2     PIC X(19)  VALUE                       
027000         'DA SPED. 1 A SPED. '.                                           
027100       05 TOT-STAMPA-PREL-ANT-SPED PIC Z(2)9.                             
027200                                                                          
027300     03 L1-HELA-STAMPA-RAD2.                                              
027400       05 FILLER                   PIC X(3)   VALUE SPACE.                
027500       05 TOT-STAMPA-PREL-R2-RUB   PIC X(23)  VALUE                       
027600               'TOTALE NUMERO BOLLE:   '.                                 
027700       05 TOT-STAMPA-R2-PREL-BOLLA PIC Z(4)9.                             
027800       05 FILLER                   PIC X(31).                             
027900       05 L1-STAMPA-TOT-DAT-DAG    PIC 9(2).                              
028000       05 FILLER                   PIC X(1)  VALUE '.'.                   
028100       05 L1-STAMPA-TOT-DAT-MAN    PIC 9(2).                              
028200       05 FILLER                   PIC X(1)  VALUE '.'.                   
028300       05 L1-STAMPA-TOT-DAT-AAR    PIC 9(2).                              
028400                                                                          
028500************** LISTA FINALE    ************                               
028600************** ALLA TRE LISTORNA *********                                
028700                                                                          
028800     03 LISTA1-FINALE-RUB1.                                               
028900       05 FILLER                   PIC X(4)   VALUE SPACE.                
029000       05 LIST-ID                  PIC X(10).                             
029100       05 FILLER                   PIC X(6)   VALUE SPACE.                
029200       05 LISTA-TOT-RUB1           PIC X(55)  VALUE                       
029300       'DISTINTA  RIEPILOGATIVA  DI  SPEDIZIONE  -  DM.29.11.78'.         
029400       05 RUB1-DEL2                PIC X(18)  VALUE                       
029500       '  ART.4  2 COMMA  '.                                              
029600       05 LISTA-TOT-DAG            PIC 9(2).                              
029700       05 FILLER                   PIC X(1)   VALUE '.'.                  
029800       05 LISTA-TOT-MAN            PIC 9(2).                              
029900       05 FILLER                   PIC X(1)   VALUE '.'.                  
030000       05 LISTA-TOT-AAR            PIC 9(2).                              
030100       05 FILLER                   PIC X(1)   VALUE SPACE.                
030200       05 LISTA-TOT-SIDA           PIC X(8)   VALUE 'PAGINA '.            
030300       05 LISTA-TOT-SIDNR          PIC Z9.                                
030400                                                                          
030500     03 LISTA1-FINALE-RUB2.                                               
030600       05 FILLER                   PIC X(3)   VALUE SPACE.                
030700       05 LISTA-TOT-RUBDEL1        PIC X(45)  VALUE                       
030800       'ARCESE TRASPORTI S.P.A                     '.                     
030900       05 LISTA-TOT-RUBDEL2        PIC X(17)  VALUE                       
031000       'NUM. RIFERIMENTO '.                                               
031100       05 LISTA-TOT-IDTRPBOR       PIC Z(4)9.                             
031200                                                                          
031300     03 LISTA1-FINALE-RUB3.                                               
031400       05 FILLER                   PIC X(3)   VALUE SPACE.                
031500       05 LISTA-TOT-RUB3           PIC X(13)  VALUE                       
031600              'SPEDIZIONIERE'.                                            
031700       05 FILLER                   PIC X(1)   VALUE SPACE.                
031800       05 LISTA-TOT-KDFRAKT        PIC Z9(2).                             
031900       05 FILLER                   PIC X(1)   VALUE SPACE.                
032000       05 LISTA-TOT-TRPNAMN        PIC X(15).                             
032100       05 FILLER                   PIC X(1)   VALUE SPACE.                
032200       05 LISTA-TOT-TRPADR1        PIC X(35).                             
032300       05 FILLER                   PIC X(1)   VALUE SPACE.                
032400       05 LISTA-TOT-TRPADR2        PIC X(35).                             
032500       05 FILLER                   PIC X(5)   VALUE SPACE.                
032600       05 LISTA-TOT-RUB3           PIC X(5)   VALUE                       
032700          'ZONA '.                                                        
032800       05 LISTA-TOT-RUB3-IDZON     PIC X(2).                              
032900                                                                          
033000     03 LISTA1-FINALE-RUB4.                                               
033100       05 FILLER                   PIC X(3)   VALUE SPACE.                
033200       05 LISTA-TOT-RUB4           PIC X(14)  VALUE                       
033300               'CONCESSIONARIO'.                                          
033400       05 FILLER                   PIC X(1)   VALUE SPACE.                
033500       05 LISTA-TOT-RUB4-KUNDNR    PIC Z(6)9.                             
033600       05 FILLER                   PIC X(1) VALUE SPACE.                  
033700       05 LISTA-TOT-RUB4-BEGODSM   PIC X(55).                             
033800       05 FILLER                   PIC X(1) VALUE SPACE.                  
033900       05 LISTA-TOT-RUB4-ADGODSMK  PIC X(35).                             
034000       05 LISTA-TOT-RUB4-SPED      PIC X(7)   VALUE                       
034100          'N.SPED '.                                                      
034200       05 LISTA-TOT-RUB4-ANTAL     PIC Z(2)9.                             
034300                                                                          
034400     03 LISTA1-FINALE-RAD-RUB.                                            
034500       05 FILLER                   PIC X(5)   VALUE SPACE.                
034600       05 LISTA-TOT-RUB1           PIC X(49)  VALUE                       
034700       'ORDINE  - DATA -   N.COLLI           PESO        '.               
034800       05 LISTA-TOT-RUB2           PIC X(22)  VALUE                       
034900       'VOLUME           BOLLA'.                                          
035000                                                                          
035100     03 LISTA1-FINALE-RADER.                                              
035200       05 FILLER                   PIC X(6)   VALUE SPACE.                
035300       05 LISTA1-RAD-IDORDNR       PIC Z(4)9.                             
035400       05 FILLER                   PIC X(3)   VALUE SPACE.                
035500       05 LISTA1-RAD-TIORDREG      PIC 9(6).                              
035600       05 FILLER                   PIC X(6)   VALUE SPACE.                
035700       05 LISTA1-RAD-KVKOLLI       PIC Z(4)9.                             
035800       05 FILLER                   PIC X(9)  VALUE SPACE.                 
035900       05 LISTA1-RAD-VKORDBTO      PIC Z(3)9.9.                           
036000       05 FILLER                   PIC X(6)   VALUE SPACE.                
036100       05 LISTA1-RAD-VLORDBTO      PIC Z(3)9.9(3).                        
036200       05 FILLER                   PIC X(8)   VALUE SPACE.                
036300       05 FILLER                   PIC X(1)   VALUE 'H'.                  
036400       05 LISTA1-RAD-IDTRPBON      PIC 9(7).                              
036500       05 FILLER                   PIC X(6)  VALUE SPACE.                 
036600       05 LISTA1-RAD-TEXT1         PIC X(12)  VALUE SPACE.                
036700       05 FILLER                   PIC X(6)  VALUE SPACE.                 
036800       05 LISTA1-RAD-TEXT2         PIC X(23)  VALUE SPACE.                
036900                                                                          
037000     03 LISTA1-FINALE-TOT-KUND.                                           
037100       05 FILLER                   PIC X(3)   VALUE SPACE.                
037200       05 LISTA1-TOT-KUND-RUB      PIC X(23)  VALUE                       
037300          'TOTALE CONCESSIONARIO  '.                                      
037400       05 LISTA1-TOT-KUND-KOLLI    PIC Z(4)9.                             
037500       05 FILLER                   PIC X(9)  VALUE SPACE.                 
037600       05 LISTA1-TOT-KUND-VIKT     PIC Z(3)9.9.                           
037700       05 FILLER                   PIC X(6)   VALUE SPACE.                
037800       05 LISTA1-TOT-KUND-VOLYM    PIC Z(3)9.9(3).                        
037900                                                                          
038000     03 LISTA1-FINALE-HELA-RAD1.                                          
038100       05 FILLER                   PIC X(3)   VALUE SPACE.                
038200       05 LISTA1-TOT-TOT-RUB1      PIC X(23)  VALUE                       
038300          'TOTALE TRASPORTATORE   '.                                      
038400       05 LISTA1-TOT-TOT-KOLLI     PIC Z(4)9.                             
038500       05 FILLER                   PIC X(9)  VALUE SPACE.                 
038600       05 LISTA1-TOT-TOT-VIKT      PIC Z(3)9.9.                           
038700       05 FILLER                   PIC X(6)   VALUE SPACE.                
038800       05 LISTA1-TOT-TOT-VOLYM     PIC Z(3)9.9(3).                        
038900       05 FILLER                   PIC X(2)   VALUE SPACE.                
039000       05 LISTA1-TOT-TOT-RUB2      PIC X(45)  VALUE                       
039100         'DATA E ORA TRASPORTO  FIRMA PER ACCETTAZIONE '.                 
039200       05 LISTA1-TOT-TOT-RUB3      PIC X(19)  VALUE                       
039300         'DA SPED. 1 A SPED. '.                                           
039400       05 LISTA1-TOT-TOT-ANTAL     PIC Z(2)9.                             
039500                                                                          
039600     03 LISTA1-FINALE-HELA-RAD2.                                          
039700       05 FILLER                   PIC X(3)   VALUE SPACE.                
039800       05 LISTA1-TOT-TOT-RUB       PIC X(23)  VALUE                       
039900          'TOTALE NUMERO BOLLE:   '.                                      
040000       05 LISTA1-TOT-TOT-BOLLA     PIC Z(4)9.                             
040100       05 FILLER                   PIC X(31)  VALUE SPACE.                
040200       05 LISTA1-TOT-TOT-DAG       PIC 9(2).                              
040300       05 FILLER                   PIC X(1)   VALUE '.'.                  
040400       05 LISTA1-TOT-TOT-MAN       PIC 9(2).                              
040500       05 FILLER                   PIC X(1)   VALUE '.'.                  
040600       05 LISTA1-TOT-TOT-AAR       PIC 9(2).                              
040700       05 FILLER                   PIC X(1)   VALUE '.'.                  
040800                                                                          
040900     03 LISTA2-FINALE-RUB1.                                               
041000       05 LISTA2-RUB1-DEL1         PIC X(44)  VALUE                       
041100       ' CODICE E RAGIONE SOCIALE CONCESSIONARIO   '.                     
041200       05 LISTA2-RUB1-DEL2         PIC X(43)  VALUE                       
041300       'NUMERO    TOTALE    TOTALE    TOTALE      '.                      
041400       05 LISTA2-RUB1-DEL3         PIC X(26)  VALUE                       
041500       'NUMERO    NUMERO    NUMERO'.                                      
041600                                                                          
041700     03 LISTA2-FINALE-RUB2.                                               
041800       05 FILLER                   PIC X(42)  VALUE SPACE.                
041900       05 LISTA2-RUB2-DEL1         PIC X(45)  VALUE                       
042000       'SPEDIZIONE   COLLI     PESO     VOLUME       '.                   
042100       05 LISTA2-RUB2-DEL2         PIC X(43)  VALUE                       
042200       ' BOLLA    ORDINE     CASSA'.                                      
042300                                                                          
042400     03 LISTA2-FINALE-RADER.                                              
042500       05 FILLER                   PIC X(1)   VALUE SPACE.                
042600       05 LISTA2-IDKUNDNR          PIC Z(6)9.                             
042700       05 FILLER                   PIC X(3)   VALUE SPACE.                
042800       05 LISTA2-BEGODSM-1         PIC X(27).                             
042900       05 FILLER                   PIC X(8)   VALUE SPACE.                
043000       05 LISTA2-ANTAL-ORDER       PIC Z(2)9.                             
043100       05 FILLER                   PIC X(6)   VALUE SPACE.                
043200       05 LISTA2-ANTAL-KOLLI       PIC Z(4)9.                             
043300       05 FILLER                   PIC X(4)   VALUE SPACE.                
043400       05 LISTA2-TOTAL-VIKT        PIC Z(3)9.9.                           
043500       05 FILLER                   PIC X(2)   VALUE SPACE.                
043600       05 LISTA2-TOTAL-VOLYM       PIC Z(4)9.9(3).                        
043700                                                                          
043800     03 LISTA3-FINALE-RUB4.                                               
043900       05 FILLER                   PIC X(3)   VALUE SPACE.                
044000       05 LISTA3-RUB               PIC X(14)  VALUE                       
044100               'CONCESSIONARIO'.                                          
044200       05 FILLER                   PIC X(1)   VALUE SPACE.                
044300       05 L3-RUB4-TOT-BEGODSM      PIC X(54).                             
044400       05 L3-RUB4-TOT-ADGODSMK     PIC X(54).                             
044500       05 L3-RUB4-TOT-ANTAL        PIC X(7)  VALUE                        
044600          'N.SPED '.                                                      
044700       05 L3-RUB4-ANT-KUND         PIC Z(2)9.                             
044800                                                                          
044900     03 LISTA3-FINALE-RUB-RAD.                                            
045000       05 FILLER                   PIC X(5)   VALUE SPACE.                
045100       05 LISTA3-RUB1-DEL1         PIC X(49)  VALUE                       
045200          'ORDINE  - DATA -   N.COLLI           PESO        '.            
045300       05 LISTA3-RUB1-DEL2         PIC X(22)  VALUE                       
045400          'VOLUME           BOLLA'.                                       
045500                                                                          
045600     03 LISTA3-FINALE-RADER.                                              
045700       05 FILLER                   PIC X(6)   VALUE SPACE.                
045800       05 LISTA3-RAD-IDORDNR       PIC Z(4)9.                             
045900       05 FILLER                   PIC X(3)   VALUE SPACE.                
046000       05 LISTA3-RAD-TIORDREG      PIC 9(6).                              
046100       05 FILLER                   PIC X(6)   VALUE SPACE.                
046200       05 LISTA3-RAD-KVKOLLI       PIC Z(4)9.                             
046300       05 FILLER                   PIC X(9)  VALUE SPACE.                 
046400       05 LISTA3-RAD-VKORDBTO      PIC Z(3)9.9.                           
046500       05 FILLER                   PIC X(6)   VALUE SPACE.                
046600       05 LISTA3-RAD-VLORDBTO      PIC Z(3)9.9(3).                        
046700       05 FILLER                   PIC X(8)   VALUE SPACE.                
046800       05 FILLER                   PIC X(1)   VALUE 'H'.                  
046900       05 LISTA3-RAD-IDTRPBON      PIC 9(7).                              
047000       05 FILLER                   PIC X(6)  VALUE SPACE.                 
047100       05 LISTA3-RAD-TEXT1         PIC X(12)  VALUE SPACE.                
047200       05 FILLER                   PIC X(6)  VALUE SPACE.                 
047300       05 LISTA3-RAD-TEXT2         PIC X(23)  VALUE SPACE.                
047400                                                                          
047500     03 LISTA3-FINALE-TOT-KUND.                                           
047600       05 FILLER                   PIC X(3)   VALUE SPACE.                
047700       05 LISTA3-TOT-KUND-RUB      PIC X(23)  VALUE                       
047800          'TOTALE CONCESSIONARIO  '.                                      
047900       05 LISTA3-TOT-KUND-KOLLI    PIC Z(4)9.                             
048000       05 FILLER                   PIC X(9)  VALUE SPACE.                 
048100       05 LISTA3-TOT-KUND-VIKT     PIC Z(3)9.9.                           
048200       05 FILLER                   PIC X(6)   VALUE SPACE.                
048300       05 LISTA3-TOT-KUND-VOLYM    PIC Z(3)9.9(3).                        
048400                                                                          
048500     03 LISTA3-FINALE-HELA-RAD1.                                          
048600       05 FILLER                   PIC X(3)   VALUE SPACE.                
048700       05 LISTA3-TOT-TOT-RUB1      PIC X(23)  VALUE                       
048800          'TOTALE TRASPORTATORE   '.                                      
048900       05 LISTA3-TOT-TOT-KOLLI     PIC Z(4)9.                             
049000       05 FILLER                   PIC X(9)  VALUE SPACE.                 
049100       05 LISTA3-TOT-TOT-VIKT      PIC Z(3)9.9.                           
049200       05 FILLER                   PIC X(6)   VALUE SPACE.                
049300       05 LISTA3-TOT-TOT-VOLYM     PIC Z(3)9.9(3).                        
049400       05 FILLER                   PIC X(2)   VALUE SPACE.                
049500       05 LISTA3-TOT-TOT-RUB2      PIC X(45)  VALUE                       
049600         'DATA E ORA TRASPORTO  FIRMA PER ACCETTAZIONE '.                 
049700       05 LISTA3-TOT-TOT-RUB3      PIC X(19)  VALUE                       
049800         'DA SPED. 1 A SPED. '.                                           
049900       05 LISTA3-TOT-TOT-ANTAL     PIC Z(2)9.                             
050000                                                                          
050100     03 LISTA3-FINALE-HELA-RAD2.                                          
050200       05 FILLER                   PIC X(3)   VALUE SPACE.                
050300       05 LISTA3-TOT-TOT-RUB       PIC X(23)  VALUE                       
050400          'TOTALE NUMERO BOLLE:   '.                                      
050500       05 LISTA3-TOT-TOT-BOLLA     PIC Z(4)9.                             
050600       05 FILLER                   PIC X(31)  VALUE SPACE.                
050700       05 LISTA3-TOT-TOT-DAG       PIC 9(2).                              
050800       05 FILLER                   PIC X(1)   VALUE '.'.                  
050900       05 LISTA3-TOT-TOT-MAN       PIC 9(2).                              
051000       05 FILLER                   PIC X(1)   VALUE '.'.                  
051100       05 LISTA3-TOT-TOT-AAR       PIC 9(2).                              
051200       05 FILLER                   PIC X(1)   VALUE '.'.                  
051300     EJECT                                                                
051400                                                                          
051500     03 LISTRAD-VCOM.                                                     
051600       05  -COPY  W406971A -PRE VCOM-                                     
051700                                                                          
051800*    03 LISTRAD-VCOM.                                                     
051900*      05 VCOM-IDTRPBOR            PIC S9(5)  VALUE ZERO COMP-3.          
052000*      05 VCOM-KDFRAKT             PIC S9(3)  VALUE ZERO COMP-3.          
052100*      05 VCOM-IDZON               PIC X(2)   VALUE SPACE.                
052200*      05 VCOM-IDKUNDNR            PIC S9(7)  VALUE ZERO COMP-3.          
052300*      05 VCOM-IDORDNR5            PIC 9(5)   VALUE ZERO.                 
052400*      05 VCOM-TIORDREG            PIC S9(7)  VALUE ZERO COMP-3.          
052500*      05 VCOM-KVKOLLI             PIC S9(3)  VALUE ZERO COMP-3.          
052600*      05 VCOM-VKORDBTO        PIC S9(6)V9(1) VALUE ZERO COMP-3.          
052700*      05 VCOM-VLORDBTO        PIC S9(4)V9(3) VALUE ZERO COMP-3.          
052800*      05 VCOM-IDTRPBO             PIC X(8)   VALUE SPACE.                
052900*      05 VCOM-TIREGDAT            PIC S9(7)  VALUE ZERO COMP-3.          
053000                                                                          
053100     EJECT                                                                
053200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
053300*                                                                         
053400     SKIP2                                                                
053500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
053600     SKIP3                                                                
053700 01  NYCKLAR-TILL-DLI.                                                    
053800                                                                          
053900     03  W-IDGMT-X.                                                       
054000         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
054100         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
054200                                                                          
054300     03  W-WDGXKEY-X.                                                     
054400         05  W-IDHTYP            PIC X(4)    VALUE '4491'.                
054500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
054600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
054700                                                                          
054800     03  W-IDLBBET-X.                                                     
054900         05  W-IDLBBET-SOEK      PIC X(12)   VALUE SPACE.                 
055000                                                                          
055100     03  W-TIAA-X.                                                        
055200         05  W-TIAA              PIC S9(3)   VALUE ZERO.                  
055300                                                                          
055400     03  W-KY4494-MIN-X.                                                  
055500         05  W-DALASTN-MIN       PIC  9(8)   VALUE ZERO.                  
055600         05  W-IDTRPTNR-MIN      PIC S9(3)   VALUE ZERO  COMP-3.          
055700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
055800                                                                          
055900     03  W-KY4494-MAX-X.                                                  
056000         05  W-DALASTN-MAX       PIC  9(8)   VALUE ZERO.                  
056100         05  W-IDTRPTNR-MAX      PIC S9(3)   VALUE ZERO  COMP-3.          
056200         05  FILLER              PIC X(24)   VALUE HIGH-VALUE.            
056300                                                                          
056400*    --- STATUS-KOD FRÅN IMS                                              
056500 01  STATUS-WS                   PIC XX.                                  
056600     88  SEGMENT-FINNS                       VALUE '  '.                  
056700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
056800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
056900     SKIP2                                                                
057000 01  GODK-STATUSKODER.                                                    
057100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
057200     SKIP3                                                                
057300 01  SSA1                        PIC X(128).                              
057400 01  SSA2                        PIC X(64).                               
057500     EJECT                                                                
057600*    --- IMS FUNKTIONSKODER                                               
057700*01  -COPY W0003                                                          
057800     EJECT                                                                
057900*    ---  DLI INPUT-OUTPUT AREA                                           
058000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
058100     SKIP3                                                                
058200 01  DLI-IO-AREA.                                                         
058300     03  IO-AREA                 PIC X(1000)  VALUE SPACE.                
058400     SKIP3                                                                
058500     03  WL449111 REDEFINES IO-AREA.                                      
058600*        05  -COPY WDGX4492  -PRE BOLLA-                                  
058700     EJECT                                                                
058800 01  DLI-IO-AREA1.                                                        
058900     03  IO-AREA1                PIC X(300)  VALUE SPACE.                 
059000     SKIP3                                                                
059100     03  WL449112 REDEFINES IO-AREA1.                                     
059200*        05  -COPY WDGX4494  -PRE BOLLA-                                  
059300     EJECT                                                                
059400 01  DLI-IO-AREA2.                                                        
059500     03  WLGMTA01.                                                        
059600*        05  -COPY WDB201                                                 
059700     EJECT                                                                
059800                                                                          
059900 LINKAGE SECTION.                                                         
060000                                                                          
060100*01  -COPY W0009   -PRE MSG-                                              
060200*01  -COPY W0009   -PRE ALT-                                              
060300*01  -COPY W0008   -PRE BOLLA-                                            
060400     05  FILLER                  PIC X.                                   
060500     EJECT                                                                
060600*01  -COPY W0008  -PRE GMTA-                                              
060700     05  FILLER                  PIC X.                                   
060800     EJECT                                                                
060900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB BOLLA-PCB GMTA-PCB.            
061000 MAIN SECTION.                                                            
061100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB BOLLA-PCB GMTA-PCB.            
061200                                                                          
061300     PERFORM IMS-GET-MSG                                                  
061400     IF SEGMENT-FINNS                                                     
061500       PERFORM A-INIT                                                     
061600       IF NYCKLAR-OK                                                      
061700         IF MID-KDSVAR = 'P'                                              
061800           PERFORM C-LAES-SKRIV-PREL                                      
061900         ELSE                                                             
062000           PERFORM D-LAES-SKRIV-TOTAL                                     
062100           IF BOLLADOK-REFNR-NY                                           
062200             PERFORM IMS-GHNP-FIRST-WL4492                                
062300             IF SEGMENT-FINNS                                             
062400               ADD +1 TO BOLLA-4492-IDTRPBOR                              
062500               PERFORM IMS-REPLACE-WL4492                                 
062600             END-IF                                                       
062700           END-IF                                                         
062800         END-IF                                                           
062900         PERFORM Z-STAENG-PRINTER                                         
063000       END-IF                                                             
063100     END-IF                                                               
063200                                                                          
063300     MOVE ZERO TO RETURN-CODE                                             
063400     GOBACK                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 A-INIT SECTION.                                                          
063800                                                                          
063900     MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W4I69701                  
064000     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
064100                                                                          
064200     ACCEPT DAGENS-DATUM FROM DATE                                        
064300                                                                          
064400     ACCEPT VCOM-DATUM   FROM DATE                                        
064500                                                                          
064600     MOVE 'AZ8'           TO WS-PRT1                                      
064700     MOVE 'W406Z1IT'      TO PRT-IDVCOM                                   
064800     MOVE 'W406971A'      TO PRT-IDCPYTXT                                 
064900     MOVE +100            TO PRC1-KVLRECL                                 
065000     MOVE SPACE           TO PRC1-IDVCINIT                                
065100     MOVE 'WIZZA1IT'      TO PRC1-TEVCOMST                                
065200                                                                          
065300     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
065400                          PRT-OPEN                                        
065500                          WS-PRT1                                         
065600                          ALT-PCB                                         
065700                          DUMMY-AREA                                      
065800                          DUMMY-AREA                                      
065900     .                                                                    
066000     EJECT                                                                
066100 C-LAES-SKRIV-PREL SECTION.                                               
066200                                                                          
066300     MOVE +0 TO WS-ANTAL-KUND                                             
066400                WS-ANTAL-BOLLA                                            
066500                                                                          
066600     MOVE MID-TIDATUM  TO W-DALASTN-MIN                                   
066700                          W-DALASTN-MAX                                   
066800     IF MID-TIDATUM NOT = ZERO                                            
066900       IF MID-TIDATUM < 500000                                            
067000         MOVE 20       TO W-DALASTN-MIN (1:2)                             
067100                          W-DALASTN-MAX (1:2)                             
067200       ELSE                                                               
067300         IF MID-TIDATUM < 999999                                          
067400           MOVE 19     TO W-DALASTN-MIN (1:2)                             
067500                          W-DALASTN-MAX (1:2)                             
067600         ELSE                                                             
067700           MOVE 99999999 TO W-DALASTN-MIN                                 
067800                            W-DALASTN-MAX                                 
067900         END-IF                                                           
068000       END-IF                                                             
068100     END-IF                                                               
068200                                                                          
068300     MOVE MID-IDTRPTNR TO W-IDTRPTNR-MIN                                  
068400                          W-IDTRPTNR-MAX                                  
068500                          WS-IDTRPTNR                                     
068600                                                                          
068700     MOVE MID-IDLBBET  TO W-IDLBBET-SOEK                                  
068800                                                                          
068900     MOVE +0    TO SPAR-IDKUNDNR                                          
069000     MOVE NEJ   TO KUND-SW                                                
069100                   ZONA-SW                                                
069200     MOVE SPACE TO SPAR-IDZON                                             
069300     MOVE MID-IDDC TO W-IDDC                                              
069400     PERFORM IMS-GHU-WL4491                                               
069500     IF SEGMENT-FINNS                                                     
069600       MOVE DAGENS-DAG  TO L1-STAMPA-RUB1-DAT-DAG                         
069700                           L1-STAMPA-TOT-DAT-DAG                          
069800       MOVE DAGENS-MAN  TO L1-STAMPA-RUB1-DAT-MAN                         
069900                           L1-STAMPA-TOT-DAT-MAN                          
070000       MOVE DAGENS-AAR  TO L1-STAMPA-RUB1-DAT-AAR                         
070100                           L1-STAMPA-TOT-DAT-AAR                          
070200                                                                          
070300       MOVE 3 TO PRT-COPIES-OVR                                           
070400                                                                          
070500       MOVE 'W40697-001' TO L1-LIST-ID                                    
070600       PERFORM CA-HAEMTA-LASTBAERARE                                      
070700       PERFORM IMS-GHNP-WL4494                                            
070800       PERFORM UNTIL SEGMENT-SAKNAS                                       
070900                                                                          
071000         IF BOLLA-4494-IDZON NOT = SPAR-IDZON                             
071100           IF NY-KUND                                                     
071200             MOVE WS-SUM-KUND-KOLLI TO TOT-PREL-ANT-KOLLI                 
071300             MOVE WS-SUM-KUND-VIKT TO TOT-PREL-VIKT                       
071400             MOVE WS-SUM-KUND-VOLYM TO TOT-PREL-VOLYM                     
071500                                                                          
071600             PERFORM S05-SKRIV-TOTKUND-STAMPA                             
071700                                                                          
071800             MOVE ZERO TO WS-SUM-KUND-KOLLI                               
071900                          WS-SUM-KUND-VIKT                                
072000                          WS-SUM-KUND-VOLYM                               
072100           END-IF                                                         
072200           MOVE BOLLA-4494-IDZON TO L1-STAMPA-RUB3-IDZON                  
072300           PERFORM S01-SKRIV-HUVRUB-STAMPA                                
072400         END-IF                                                           
072500                                                                          
072600                                                                          
072700         IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                       
072800                                                                          
072900           IF NY-KUND                                                     
073000             IF GAMMAL-ZONA                                               
073100               MOVE WS-SUM-KUND-KOLLI TO TOT-PREL-ANT-KOLLI               
073200               MOVE WS-SUM-KUND-VIKT TO TOT-PREL-VIKT                     
073300               MOVE WS-SUM-KUND-VOLYM TO TOT-PREL-VOLYM                   
073400                                                                          
073500               PERFORM S05-SKRIV-TOTKUND-STAMPA                           
073600                                                                          
073700               MOVE ZERO TO WS-SUM-KUND-KOLLI                             
073800                            WS-SUM-KUND-VIKT                              
073900                            WS-SUM-KUND-VOLYM                             
074000             END-IF                                                       
074100           END-IF                                                         
074200                                                                          
074300           PERFORM S10-HAEMTA-KUND-UPPGIFT                                
074400           MOVE WS-ANTAL-KUND     TO L1-RUB4-ANT-KUND                     
074500           PERFORM S03-SKRIV-KUND-RAD-STAMPA                              
074600                                                                          
074700           PERFORM CC-FLYTTA-TILL-LISTA                                   
074800           PERFORM S04-SKRIV-RADER-STAMPA                                 
074900           PERFORM CD-SUMMERA-TOTALER                                     
075000         ELSE                                                             
075100           PERFORM CC-FLYTTA-TILL-LISTA                                   
075200           PERFORM S04-SKRIV-RADER-STAMPA                                 
075300           PERFORM CD-SUMMERA-TOTALER                                     
075400         END-IF                                                           
075500                                                                          
075600         MOVE BOLLA-4494-IDZON TO SPAR-IDZON                              
075700         MOVE BOLLA-4494-IDKUNDNR TO SPAR-IDKUNDNR                        
075800                                                                          
075900         PERFORM IMS-GHNP-WL4494                                          
076000                                                                          
076100         IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                       
076200           MOVE JA TO KUND-SW                                             
076300         ELSE                                                             
076400           MOVE NEJ TO KUND-SW                                            
076500         END-IF                                                           
076600         IF BOLLA-4494-IDZON NOT = SPAR-IDZON                             
076700           MOVE JA TO ZONA-SW                                             
076800         ELSE                                                             
076900           MOVE NEJ TO ZONA-SW                                            
077000         END-IF                                                           
077100       END-PERFORM                                                        
077200                                                                          
077300       MOVE WS-SUM-KUND-KOLLI TO TOT-PREL-ANT-KOLLI                       
077400       MOVE WS-SUM-KUND-VIKT  TO TOT-PREL-VIKT                            
077500       MOVE WS-SUM-KUND-VOLYM TO TOT-PREL-VOLYM                           
077600                                                                          
077700       PERFORM S05-SKRIV-TOTKUND-STAMPA                                   
077800                                                                          
077900       MOVE ZERO TO WS-SUM-KUND-KOLLI                                     
078000                    WS-SUM-KUND-VIKT                                      
078100                    WS-SUM-KUND-VOLYM                                     
078200                                                                          
078300       MOVE WS-SUM-TOT-KOLLI TO TOT-STAMPA-PREL-KOLLI                     
078400       MOVE WS-SUM-TOT-VIKT  TO TOT-STAMPA-PREL-VIKT                      
078500       MOVE WS-SUM-TOT-VOLYM TO TOT-STAMPA-PREL-VOLYM                     
078600       MOVE WS-ANTAL-KUND    TO TOT-STAMPA-PREL-ANT-SPED                  
078700       MOVE WS-ANTAL-BOLLA   TO TOT-STAMPA-R2-PREL-BOLLA                  
078800                                                                          
078900       PERFORM S06-SKRIV-TOTAL-STAMPA                                     
079000       MOVE ZERO TO WS-SUM-TOT-KOLLI                                      
079100                    WS-SUM-TOT-VIKT                                       
079200                    WS-SUM-TOT-VOLYM                                      
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600                                                                          
079700 CA-HAEMTA-LASTBAERARE SECTION.                                           
079800                                                                          
079900     PERFORM IMS-GHNP-FIRST-WL4492                                        
080000     IF SEGMENT-FINNS                                                     
080100       MOVE +1 TO INDX                                                    
080200       MOVE NEJ TO TRAFF-SW                                               
080300                                                                          
080400       PERFORM UNTIL (INDX > MAX-INDX)  OR TRAFF                          
080500         IF BOLLA-4492-IDTRPTNR (INDX) = WS-IDTRPTNR                      
080600           MOVE BOLLA-4492-BETRPFIR (INDX) TO                             
080700                L1-STAMPA-RUB3-TRPNAMN                                    
080800                LISTA-TOT-TRPNAMN                                         
080900           MOVE BOLLA-4492-ADTRPFIR-RAD1 (INDX) TO                        
081000                L1-STAMPA-RUB3-TRPADR1                                    
081100                LISTA-TOT-TRPADR1                                         
081200           MOVE BOLLA-4492-ADTRPFIR-RAD2 (INDX) TO                        
081300                L1-STAMPA-RUB3-TRPADR2                                    
081400                LISTA-TOT-TRPADR2                                         
081500           MOVE BOLLA-4492-KDFRAKT       (INDX) TO                        
081600                L1-STAMPA-RUB3-KDFRAKT                                    
081700                LISTA-TOT-KDFRAKT                                         
081800                WS-KDFRAKT                                                
081900           MOVE JA TO TRAFF-SW                                            
082000         ELSE                                                             
082100           MOVE 'TRP SAKNAS' TO                                           
082200                L1-STAMPA-RUB3-TRPNAMN                                    
082300                LISTA-TOT-TRPNAMN                                         
082400         END-IF                                                           
082500                                                                          
082600         ADD +1 TO INDX                                                   
082700       END-PERFORM                                                        
082800                                                                          
082900*SECT CA- ANV. I BÅDE C-LAES OCH D-LAES                                   
083000*      IF MID-KDSVAR = 'P'                                                
083100*        MOVE BOLLA-4492-IDTRPBOR TO WS-IDTRPBOR                          
083200*      ELSE                                                               
083300*        IF BOLLA-4494-IDTRPBOR = ZERO                                    
083400*          MOVE BOLLA-4492-IDTRPBOR TO WS-IDTRPBOR                        
083500*        END-IF                                                           
083600*      END-IF                                                             
083700     END-IF                                                               
083800     .                                                                    
083900     EJECT                                                                
084000 CC-FLYTTA-TILL-LISTA SECTION.                                            
084100                                                                          
084200     MOVE BOLLA-4494-IDORDNR7 TO L1-STAMPA-IDORDNR                        
084300     MOVE BOLLA-4494-TIORDREG TO L1-STAMPA-TIORDREG                       
084400     MOVE BOLLA-4494-KVKOLLI  TO L1-STAMPA-KVKOLLI                        
084500     MOVE BOLLA-4494-VKORDBTO TO L1-STAMPA-VKORDBTO                       
084600     MOVE BOLLA-4494-VLORDBTO TO L1-STAMPA-VLORDBTO                       
084700     MOVE BOLLA-4494-IDTRPBON TO L1-STAMPA-IDTRPBON                       
084800                                                                          
084900     IF WS-IDTRPTNR = 300                                                 
085000       MOVE 'PORTO FRANCO ' TO L1-STAMPA-TEXT1                            
085100     ELSE                                                                 
085200       MOVE SPACE           TO L1-STAMPA-TEXT1                            
085300     END-IF                                                               
085400                                                                          
085500     ADD +1 TO WS-ANTAL-BOLLA                                             
085600     .                                                                    
085700     EJECT                                                                
085800                                                                          
085900 CD-SUMMERA-TOTALER SECTION.                                              
086000                                                                          
086100     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-KUND-KOLLI                        
086200     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-KUND-VIKT                         
086300     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-KUND-VOLYM                        
086400                                                                          
086500     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-TOT-KOLLI                         
086600     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-TOT-VIKT                          
086700     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-TOT-VOLYM                         
086800                                                                          
086900     .                                                                    
087000     EJECT                                                                
087100 D-LAES-SKRIV-TOTAL SECTION.                                              
087200                                                                          
087300     MOVE +0 TO WS-ANTAL-KUND                                             
087400                WS-ANTAL-BOLLA                                            
087500                                                                          
087600     MOVE MID-TIDATUM  TO W-DALASTN-MIN                                   
087700                          W-DALASTN-MAX                                   
087800     IF MID-TIDATUM NOT = ZERO                                            
087900       IF MID-TIDATUM < 500000                                            
088000         MOVE 20       TO W-DALASTN-MIN (1:2)                             
088100                          W-DALASTN-MAX (1:2)                             
088200       ELSE                                                               
088300         IF MID-TIDATUM < 999999                                          
088400           MOVE 19     TO W-DALASTN-MIN (1:2)                             
088500                          W-DALASTN-MAX (1:2)                             
088600         ELSE                                                             
088700           MOVE 99999999 TO W-DALASTN-MIN                                 
088800                            W-DALASTN-MAX                                 
088900         END-IF                                                           
089000       END-IF                                                             
089100     END-IF                                                               
089200     MOVE MID-IDTRPTNR TO W-IDTRPTNR-MIN                                  
089300                          W-IDTRPTNR-MAX                                  
089400                          WS-IDTRPTNR                                     
089500                                                                          
089600     CALL W006PRC1 USING PRT-VCOM                                         
089700                          PRT-OPEN                                        
089800                          WS-PRT2                                         
089900                          ALT-PCB                                         
090000                          PRC1-W006PRVC                                   
090100                                                                          
090200     MOVE NEJ TO KUND-SW                                                  
090300                 ZONA-SW                                                  
090400                                                                          
090500     MOVE SPACE TO SPAR-IDZON                                             
090600     MOVE ZERO  TO SPAR-IDKUNDNR                                          
090700     MOVE MID-IDDC TO W-IDDC                                              
090800                                                                          
090900     PERFORM IMS-GHU-WL4491                                               
091000     IF SEGMENT-FINNS                                                     
091100                                                                          
091200                                                                          
091300       PERFORM CA-HAEMTA-LASTBAERARE                                      
091400       PERFORM IMS-GHNP-WL4494-TRP                                        
091500                                                                          
091600       IF BOLLA-4494-IDTRPBOR = ZERO                                      
091700*4492-IDTRPBOR LÄSES IN I CA-HAEMTA-LASTBAE 4 RADER TIDGARE               
091800         MOVE BOLLA-4492-IDTRPBOR  TO WS-IDTRPBOR                         
091900         MOVE JA TO VCOM-SW                                               
092000                    BOLLADOK-SW                                           
092100       ELSE                                                               
092200         MOVE BOLLA-4494-IDTRPBOR  TO WS-IDTRPBOR                         
092300         MOVE NEJ TO VCOM-SW                                              
092400                     BOLLADOK-SW                                          
092500       END-IF                                                             
092600                                                                          
092700*VCOM-START                                                               
092800       MOVE DAGENS-AAR TO LISTA-TOT-AAR                                   
092900                          LISTA1-TOT-TOT-AAR                              
093000                          LISTA3-TOT-TOT-AAR                              
093100       MOVE DAGENS-MAN TO LISTA-TOT-MAN                                   
093200                          LISTA1-TOT-TOT-MAN                              
093300                          LISTA3-TOT-TOT-MAN                              
093400       MOVE DAGENS-DAG TO LISTA-TOT-DAG                                   
093500                          LISTA1-TOT-TOT-DAG                              
093600                          LISTA3-TOT-TOT-DAG                              
093700                                                                          
093800************   FÖRST SKRIVS VCOMFIL TILL SUSA (ALLA TRPTNR)               
093900                                                                          
094000         PERFORM UNTIL SEGMENT-SAKNAS                                     
094100           IF VCOM-SKRIVS                                                 
094200             IF BOLLA-4494-IDTRPBOR = ZERO                                
094300               MOVE WS-IDTRPBOR            TO VCOM-IDTRPBOR               
094400             ELSE                                                         
094500               MOVE BOLLA-4494-IDTRPBOR    TO VCOM-IDTRPBOR               
094600             END-IF                                                       
094700             MOVE WS-KDFRAKT               TO VCOM-KDFRAKT                
094800             MOVE BOLLA-4494-IDZON         TO VCOM-IDZON                  
094900             MOVE BOLLA-4494-IDKUNDNR      TO VCOM-IDKUNDNR               
095000             MOVE BOLLA-4494-IDORDNR7(3:5) TO VCOM-IDORDNR5               
095100             MOVE BOLLA-4494-TIORDREG      TO VCOM-TIORDREG               
095200             MOVE BOLLA-4494-KVKOLLI       TO VCOM-KVKOLLI                
095300             MOVE BOLLA-4494-VKORDBTO      TO VCOM-VKORDBTO               
095400             MOVE BOLLA-4494-VLORDBTO      TO VCOM-VLORDBTO               
095500             MOVE 'H'                      TO WS-IDTRPBOT                 
095600             MOVE BOLLA-4494-IDTRPBON      TO WS-IDTRPBON                 
095700             MOVE WS-BOLLANR               TO VCOM-IDTRPBO                
095800             MOVE VCOM-DATUM               TO VCOM-TIREGDAT               
095900             PERFORM S99-SKRIV-VCOM-FIL                                   
096000           END-IF                                                         
096100           PERFORM IMS-GHNP-WL4494-TRP                                    
096200         END-PERFORM                                                      
096300                                                                          
096400*********************   BÖRJAR OM FÖR ATT SKRIVA LISTOR                   
096500       IF MID-IDTRPTNR = 100                                              
096600*************** HÄR SKRIVS LISTA1 FINALE                                  
096700                                                                          
096800         PERFORM IMS-GHNP-WL4494-TRP-FIRST                                
096900         MOVE 1 TO PRT-COPIES-OVR                                         
097000         MOVE 'W40697-002' TO LIST-ID                                     
097100                                                                          
097200         PERFORM UNTIL SEGMENT-SAKNAS                                     
097300                                                                          
097400           IF BOLLA-4494-IDZON NOT = SPAR-IDZON                           
097500             IF GAMMAL-KUND                                               
097600               MOVE BOLLA-4494-IDZON TO LISTA-TOT-RUB3-IDZON              
097700               MOVE WS-IDTRPBOR  TO LISTA-TOT-IDTRPBOR                    
097800               PERFORM S11-SKRIV-RUB-LISTA1                               
097900             END-IF                                                       
098000           END-IF                                                         
098100                                                                          
098200           IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                     
098300             IF NY-KUND                                                   
098400               MOVE WS-SUM-KUND-KOLLI TO LISTA1-TOT-KUND-KOLLI            
098500                                                                          
098600               MOVE WS-SUM-KUND-VIKT TO LISTA1-TOT-KUND-VIKT              
098700                                                                          
098800               MOVE WS-SUM-KUND-VOLYM TO LISTA1-TOT-KUND-VOLYM            
098900                                                                          
099000               PERFORM S15-SKRIV-TOTKUND-LISTA1                           
099100                                                                          
099200               MOVE BOLLA-4494-IDZON TO LISTA-TOT-RUB3-IDZON              
099300               PERFORM S11-SKRIV-RUB-LISTA1                               
099400                                                                          
099500               MOVE ZERO            TO WS-SUM-KUND-KOLLI                  
099600                                         WS-SUM-KUND-VIKT                 
099700                                         WS-SUM-KUND-VOLYM                
099800                                         WS-ANTAL-ORDER                   
099900             END-IF                                                       
100000                                                                          
100100             PERFORM S10-HAEMTA-KUND-UPPGIFT                              
100200             MOVE WS-ANTAL-KUND TO LISTA-TOT-RUB4-ANTAL                   
100300             PERFORM S13-SKRIV-KUND-RAD-LISTA1                            
100400                                                                          
100500             PERFORM DB-FLYTTA-TILL-LISTTOT1                              
100600             PERFORM S14-SKRIV-RADER-LISTA1                               
100700             PERFORM DC-SUMMERA-TOTALER                                   
100800             IF BOLLA-4494-IDTRPBOR = ZERO                                
100900               MOVE WS-IDTRPBOR TO BOLLA-4494-IDTRPBOR                    
101000               PERFORM IMS-REPLACE-WL4494                                 
101100             END-IF                                                       
101200           ELSE                                                           
101300             PERFORM DB-FLYTTA-TILL-LISTTOT1                              
101400             PERFORM S14-SKRIV-RADER-LISTA1                               
101500             PERFORM DC-SUMMERA-TOTALER                                   
101600             IF BOLLA-4494-IDTRPBOR = ZERO                                
101700               MOVE WS-IDTRPBOR TO BOLLA-4494-IDTRPBOR                    
101800               PERFORM IMS-REPLACE-WL4494                                 
101900             END-IF                                                       
102000           END-IF                                                         
102100                                                                          
102200           MOVE BOLLA-4494-IDKUNDNR TO SPAR-IDKUNDNR                      
102300           MOVE BOLLA-4494-IDZON      TO SPAR-IDZON                       
102400                                                                          
102500                                                                          
102600           PERFORM IMS-GHNP-WL4494-TRP                                    
102700                                                                          
102800           IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                     
102900             MOVE JA TO KUND-SW                                           
103000           ELSE                                                           
103100             MOVE NEJ TO KUND-SW                                          
103200           END-IF                                                         
103300         END-PERFORM                                                      
103400                                                                          
103500         MOVE WS-SUM-KUND-KOLLI TO LISTA1-TOT-KUND-KOLLI                  
103600         MOVE WS-SUM-KUND-VIKT TO LISTA1-TOT-KUND-VIKT                    
103700         MOVE WS-SUM-KUND-VOLYM TO LISTA1-TOT-KUND-VOLYM                  
103800                                                                          
103900         PERFORM S15-SKRIV-TOTKUND-LISTA1                                 
104000                                                                          
104100         MOVE ZERO      TO WS-SUM-KUND-KOLLI                              
104200                             WS-SUM-KUND-VIKT                             
104300                             WS-SUM-KUND-VOLYM                            
104400                             WS-ANTAL-ORDER                               
104500                                                                          
104600         MOVE WS-SUM-TOT-KOLLI        TO LISTA1-TOT-TOT-KOLLI             
104700         MOVE WS-SUM-TOT-VIKT         TO LISTA1-TOT-TOT-VIKT              
104800         MOVE WS-SUM-TOT-VOLYM        TO LISTA1-TOT-TOT-VOLYM             
104900         MOVE WS-ANTAL-KUND           TO LISTA1-TOT-TOT-ANTAL             
105000         MOVE WS-ANTAL-BOLLA          TO LISTA1-TOT-TOT-BOLLA             
105100                                                                          
105200         PERFORM S16-SKRIV-TOTAL-LISTA1                                   
105300                                                                          
105400         MOVE ZERO      TO WS-SUM-TOT-KOLLI                               
105500                             WS-SUM-TOT-VIKT                              
105600                             WS-SUM-TOT-VOLYM                             
105700                                                                          
105800*********************   BÖRJAR OM FÖR ATT SKRIVA LISTA2                   
105900                                                                          
106000         MOVE 'W40697-003' TO LIST-ID                                     
106100         MOVE SPACE TO SPAR-IDZON                                         
106200         MOVE +0  TO WS-ANTAL-ORDER                                       
106300                       SPAR-IDKUNDNR                                      
106400                                                                          
106500         MOVE ZERO TO  WS-SUM-KUND-KOLLI                                  
106600                       WS-SUM-KUND-VIKT                                   
106700                       WS-SUM-KUND-VOLYM                                  
106800                                                                          
106900         MOVE NEJ TO KUND-SW                                              
107000                     ZONA-SW                                              
107100                                                                          
107200         PERFORM IMS-GHNP-WL4494-TRP-FIRST                                
107300                                                                          
107400         PERFORM UNTIL SEGMENT-SAKNAS                                     
107500                                                                          
107600           IF BOLLA-4494-IDZON NOT = SPAR-IDZON                           
107700             IF NY-KUND                                                   
107800               MOVE WS-ANTAL-ORDER TO LISTA2-ANTAL-ORDER                  
107900               MOVE WS-IDKUNDNR   TO LISTA2-IDKUNDNR                      
108000               MOVE WS-SUM-KUND-KOLLI TO LISTA2-ANTAL-KOLLI               
108100               MOVE WS-SUM-KUND-VIKT TO LISTA2-TOTAL-VIKT                 
108200               MOVE WS-SUM-KUND-VOLYM TO LISTA2-TOTAL-VOLYM               
108300                                                                          
108400               PERFORM S24-SKRIV-RADER-LISTA2                             
108500               MOVE PRT-AFTER-1 TO PRT-RADSKIP                            
108600                                                                          
108700               MOVE ZERO            TO WS-SUM-KUND-KOLLI                  
108800                                           WS-SUM-KUND-VIKT               
108900                                           WS-SUM-KUND-VOLYM              
109000                                           WS-ANTAL-ORDER                 
109100             END-IF                                                       
109200             MOVE BOLLA-4494-IDZON TO LISTA-TOT-RUB3-IDZON                
109300                                                                          
109400             PERFORM S21-SKRIV-RUB-LISTA2                                 
109500             MOVE PRT-AFTER-2 TO PRT-RADSKIP                              
109600           END-IF                                                         
109700                                                                          
109800           IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                     
109900             IF NY-KUND                                                   
110000               IF GAMMAL-ZONA                                             
110100                 MOVE WS-ANTAL-ORDER TO LISTA2-ANTAL-ORDER                
110200                 MOVE WS-IDKUNDNR TO LISTA2-IDKUNDNR                      
110300                 MOVE WS-SUM-KUND-KOLLI TO LISTA2-ANTAL-KOLLI             
110400                 MOVE WS-SUM-KUND-VIKT TO LISTA2-TOTAL-VIKT               
110500                 MOVE WS-SUM-KUND-VOLYM TO LISTA2-TOTAL-VOLYM             
110600                                                                          
110700                 PERFORM S24-SKRIV-RADER-LISTA2                           
110800                 MOVE PRT-AFTER-1 TO PRT-RADSKIP                          
110900                                                                          
111000                 MOVE ZERO          TO WS-SUM-KUND-KOLLI                  
111100                                             WS-SUM-KUND-VIKT             
111200                                             WS-SUM-KUND-VOLYM            
111300                                             WS-ANTAL-ORDER               
111400               END-IF                                                     
111500             END-IF                                                       
111600                                                                          
111700             PERFORM S10-HAEMTA-KUND-UPPGIFT                              
111800             PERFORM DD-SUMMERA-TOTALER                                   
111900                                                                          
112000           ELSE                                                           
112100             PERFORM DD-SUMMERA-TOTALER                                   
112200           END-IF                                                         
112300                                                                          
112400           MOVE BOLLA-4494-IDKUNDNR TO SPAR-IDKUNDNR                      
112500           MOVE BOLLA-4494-IDZON      TO SPAR-IDZON                       
112600                                                                          
112700                                                                          
112800           PERFORM IMS-GHNP-WL4494-TRP                                    
112900           IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                     
113000             MOVE JA TO KUND-SW                                           
113100           ELSE                                                           
113200             MOVE NEJ TO KUND-SW                                          
113300           END-IF                                                         
113400                                                                          
113500           IF BOLLA-4494-IDZON NOT = SPAR-IDZON                           
113600             MOVE JA TO ZONA-SW                                           
113700           ELSE                                                           
113800             MOVE NEJ TO ZONA-SW                                          
113900           END-IF                                                         
114000                                                                          
114100         END-PERFORM                                                      
114200                                                                          
114300         MOVE WS-ANTAL-ORDER      TO LISTA2-ANTAL-ORDER                   
114400         MOVE WS-IDKUNDNR         TO LISTA2-IDKUNDNR                      
114500         MOVE WS-SUM-KUND-KOLLI TO LISTA2-ANTAL-KOLLI                     
114600         MOVE WS-SUM-KUND-VIKT TO LISTA2-TOTAL-VIKT                       
114700         MOVE WS-SUM-KUND-VOLYM TO LISTA2-TOTAL-VOLYM                     
114800                                                                          
114900         PERFORM S24-SKRIV-RADER-LISTA2                                   
115000         MOVE PRT-AFTER-1 TO PRT-RADSKIP                                  
115100                                                                          
115200         MOVE ZERO                  TO WS-SUM-KUND-KOLLI                  
115300                                     WS-SUM-KUND-VIKT                     
115400                                     WS-SUM-KUND-VOLYM                    
115500                                     WS-ANTAL-ORDER                       
115600                                                                          
115700         MOVE ZERO      TO WS-SUM-TOT-KOLLI                               
115800                             WS-SUM-TOT-VIKT                              
115900                             WS-SUM-TOT-VOLYM                             
116000       END-IF                                                             
116100                                                                          
116200********************* BÖRJAR OM FÖR ATT SKRIVA LISTA3                     
116300                                                                          
116400     PERFORM ZA-STAENG-PRINTER                                            
116500                                                                          
116600     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
116700                          PRT-OPEN                                        
116800                          WS-PRT1                                         
116900                          ALT-PCB                                         
117000                          DUMMY-AREA                                      
117100                          DUMMY-AREA                                      
117200                                                                          
117300       IF MID-IDTRPTNR = 100                                              
117400         MOVE 3 TO PRT-COPIES-OVR                                         
117500       ELSE                                                               
117600         IF MID-IDTRPTNR = 200                                            
117700           MOVE 3 TO PRT-COPIES-OVR                                       
117800         ELSE                                                             
117900           MOVE 1 TO PRT-COPIES-OVR                                       
118000         END-IF                                                           
118100       END-IF                                                             
118200                                                                          
118300                                                                          
118400       MOVE 'W40697-004' TO LIST-ID                                       
118500       MOVE SPACE TO SPAR-IDZON                                           
118600       MOVE NEJ TO KUND-SW                                                
118700                   ZONA-SW                                                
118800       MOVE +0    TO SPAR-IDKUNDNR                                        
118900                     WS-ANTAL-BOLLA                                       
119000                     WS-ANTAL-KUND                                        
119100                     WS-SUM-KUND-KOLLI                                    
119200                     WS-SUM-KUND-VIKT                                     
119300                     WS-SUM-KUND-VOLYM                                    
119400                     WS-ANTAL-ORDER                                       
119500                                                                          
119600       PERFORM IMS-GHNP-WL4494-TRP-FIRST                                  
119700                                                                          
119800       PERFORM UNTIL SEGMENT-SAKNAS                                       
119900                                                                          
120000                                                                          
120100         IF BOLLA-4494-IDZON NOT = SPAR-IDZON                             
120200           IF NY-KUND                                                     
120300             MOVE WS-SUM-KUND-KOLLI TO LISTA3-TOT-KUND-KOLLI              
120400                                                                          
120500             MOVE WS-SUM-KUND-VIKT  TO LISTA3-TOT-KUND-VIKT               
120600                                                                          
120700             MOVE WS-SUM-KUND-VOLYM TO LISTA3-TOT-KUND-VOLYM              
120800                                                                          
120900             PERFORM S35-SKRIV-TOTKUND-LISTA3                             
121000                                                                          
121100             MOVE ZERO              TO WS-SUM-KUND-KOLLI                  
121200                                       WS-SUM-KUND-VIKT                   
121300                                       WS-SUM-KUND-VOLYM                  
121400                                       WS-ANTAL-ORDER                     
121500           END-IF                                                         
121600           MOVE BOLLA-4494-IDZON TO LISTA-TOT-RUB3-IDZON                  
121700           MOVE WS-IDTRPBOR      TO LISTA-TOT-IDTRPBOR                    
121800           PERFORM S31-SKRIV-RUB-LISTA3                                   
121900         END-IF                                                           
122000                                                                          
122100         IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                       
122200                                                                          
122300           IF NY-KUND                                                     
122400             IF GAMMAL-ZONA                                               
122500               MOVE WS-SUM-KUND-KOLLI TO LISTA3-TOT-KUND-KOLLI            
122600                                                                          
122700               MOVE WS-SUM-KUND-VIKT TO LISTA3-TOT-KUND-VIKT              
122800                                                                          
122900               MOVE WS-SUM-KUND-VOLYM TO LISTA3-TOT-KUND-VOLYM            
123000                                                                          
123100               PERFORM S35-SKRIV-TOTKUND-LISTA3                           
123200                                                                          
123300               MOVE ZERO            TO WS-SUM-KUND-KOLLI                  
123400                                         WS-SUM-KUND-VIKT                 
123500                                         WS-SUM-KUND-VOLYM                
123600                                         WS-ANTAL-ORDER                   
123700             END-IF                                                       
123800           END-IF                                                         
123900                                                                          
124000           PERFORM S10-HAEMTA-KUND-UPPGIFT                                
124100           MOVE WS-ANTAL-KUND TO LISTA-TOT-RUB4-ANTAL                     
124200           PERFORM S33-SKRIV-KUND-RAD-LISTA3                              
124300                                                                          
124400           PERFORM DE-FLYTTA-TILL-LISTA3                                  
124500           PERFORM S34-SKRIV-RADER-LISTA3                                 
124600           PERFORM DF-SUMMERA-TOTALER                                     
124700           IF BOLLA-4494-IDTRPBOR = ZERO                                  
124800             MOVE WS-IDTRPBOR TO BOLLA-4494-IDTRPBOR                      
124900             PERFORM IMS-REPLACE-WL4494                                   
125000           END-IF                                                         
125100         ELSE                                                             
125200           PERFORM DE-FLYTTA-TILL-LISTA3                                  
125300           PERFORM S34-SKRIV-RADER-LISTA3                                 
125400           PERFORM DF-SUMMERA-TOTALER                                     
125500           IF BOLLA-4494-IDTRPBOR = ZERO                                  
125600             MOVE WS-IDTRPBOR TO BOLLA-4494-IDTRPBOR                      
125700             PERFORM IMS-REPLACE-WL4494                                   
125800           END-IF                                                         
125900         END-IF                                                           
126000                                                                          
126100         MOVE BOLLA-4494-IDKUNDNR TO SPAR-IDKUNDNR                        
126200         MOVE BOLLA-4494-IDZON    TO SPAR-IDZON                           
126300                                                                          
126400         PERFORM IMS-GHNP-WL4494-TRP                                      
126500                                                                          
126600         IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                       
126700           MOVE JA TO KUND-SW                                             
126800         ELSE                                                             
126900           MOVE NEJ TO KUND-SW                                            
127000         END-IF                                                           
127100                                                                          
127200         IF BOLLA-4494-IDZON NOT = SPAR-IDZON                             
127300           MOVE JA TO ZONA-SW                                             
127400         ELSE                                                             
127500           MOVE NEJ TO ZONA-SW                                            
127600         END-IF                                                           
127700                                                                          
127800       END-PERFORM                                                        
127900                                                                          
128000       MOVE WS-SUM-KUND-KOLLI TO LISTA3-TOT-KUND-KOLLI                    
128100       MOVE WS-SUM-KUND-VIKT  TO LISTA3-TOT-KUND-VIKT                     
128200       MOVE WS-SUM-KUND-VOLYM TO LISTA3-TOT-KUND-VOLYM                    
128300                                                                          
128400       PERFORM S35-SKRIV-TOTKUND-LISTA3                                   
128500                                                                          
128600       MOVE ZERO              TO WS-SUM-KUND-KOLLI                        
128700                                 WS-SUM-KUND-VIKT                         
128800                                 WS-SUM-KUND-VOLYM                        
128900                                 WS-ANTAL-ORDER                           
129000                                                                          
129100       MOVE WS-SUM-TOT-KOLLI  TO LISTA3-TOT-TOT-KOLLI                     
129200       MOVE WS-SUM-TOT-VIKT   TO LISTA3-TOT-TOT-VIKT                      
129300       MOVE WS-SUM-TOT-VOLYM  TO LISTA3-TOT-TOT-VOLYM                     
129400       MOVE WS-ANTAL-KUND     TO LISTA3-TOT-TOT-ANTAL                     
129500       MOVE WS-ANTAL-BOLLA    TO LISTA3-TOT-TOT-BOLLA                     
129600                                                                          
129700       PERFORM S36-SKRIV-TOTAL-LISTA3                                     
129800                                                                          
129900       MOVE ZERO        TO WS-SUM-TOT-KOLLI                               
130000                           WS-SUM-TOT-VIKT                                
130100                           WS-SUM-TOT-VOLYM                               
130200     END-IF                                                               
130300     .                                                                    
130400     EJECT                                                                
130500 DB-FLYTTA-TILL-LISTTOT1 SECTION.                                         
130600                                                                          
130700     MOVE BOLLA-4494-IDORDNR7 TO LISTA1-RAD-IDORDNR                       
130800                                 LISTA3-RAD-IDORDNR                       
130900                                                                          
131000     MOVE BOLLA-4494-TIORDREG TO LISTA1-RAD-TIORDREG                      
131100                                                                          
131200     MOVE BOLLA-4494-KVKOLLI  TO LISTA1-RAD-KVKOLLI                       
131300                                                                          
131400     MOVE BOLLA-4494-VKORDBTO TO LISTA1-RAD-VKORDBTO                      
131500                                                                          
131600     MOVE BOLLA-4494-VLORDBTO TO LISTA1-RAD-VLORDBTO                      
131700                                                                          
131800     MOVE BOLLA-4494-IDTRPBON TO LISTA1-RAD-IDTRPBON                      
131900                                                                          
132000     IF WS-IDTRPTNR = 300                                                 
132100       MOVE 'PORTO FRANCO ' TO LISTA1-RAD-TEXT1                           
132200     ELSE                                                                 
132300       MOVE SPACE           TO LISTA1-RAD-TEXT1                           
132400     END-IF                                                               
132500                                                                          
132600     ADD +1 TO WS-ANTAL-BOLLA                                             
132700     .                                                                    
132800     EJECT                                                                
132900                                                                          
133000 DC-SUMMERA-TOTALER SECTION.                                              
133100                                                                          
133200     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-KUND-KOLLI                        
133300     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-KUND-VIKT                         
133400     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-KUND-VOLYM                        
133500                                                                          
133600     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-TOT-KOLLI                         
133700     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-TOT-VIKT                          
133800     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-TOT-VOLYM                         
133900                                                                          
134000     ADD +1 TO WS-ANTAL-ORDER                                             
134100     .                                                                    
134200     EJECT                                                                
134300                                                                          
134400 DD-SUMMERA-TOTALER SECTION.                                              
134500                                                                          
134600     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-KUND-KOLLI                        
134700     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-KUND-VIKT                         
134800     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-KUND-VOLYM                        
134900                                                                          
135000     ADD +1 TO WS-ANTAL-ORDER                                             
135100                                                                          
135200     .                                                                    
135300     EJECT                                                                
135400 DE-FLYTTA-TILL-LISTA3 SECTION.                                           
135500                                                                          
135600     MOVE BOLLA-4494-IDORDNR7 TO LISTA3-RAD-IDORDNR                       
135700                                                                          
135800     MOVE BOLLA-4494-TIORDREG TO LISTA3-RAD-TIORDREG                      
135900                                                                          
136000     MOVE BOLLA-4494-KVKOLLI  TO LISTA3-RAD-KVKOLLI                       
136100                                                                          
136200     MOVE BOLLA-4494-VKORDBTO TO LISTA3-RAD-VKORDBTO                      
136300                                                                          
136400     MOVE BOLLA-4494-VLORDBTO TO LISTA3-RAD-VLORDBTO                      
136500                                                                          
136600     MOVE BOLLA-4494-IDTRPBON TO LISTA3-RAD-IDTRPBON                      
136700                                                                          
136800     IF WS-IDTRPTNR = 300                                                 
136900       MOVE 'PORTO FRANCO ' TO LISTA3-RAD-TEXT1                           
137000     ELSE                                                                 
137100       MOVE SPACE           TO LISTA3-RAD-TEXT1                           
137200     END-IF                                                               
137300                                                                          
137400                                                                          
137500     ADD +1 TO WS-ANTAL-BOLLA                                             
137600     .                                                                    
137700     EJECT                                                                
137800                                                                          
137900 DF-SUMMERA-TOTALER SECTION.                                              
138000                                                                          
138100     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-KUND-KOLLI                        
138200     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-KUND-VIKT                         
138300     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-KUND-VOLYM                        
138400                                                                          
138500     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-TOT-KOLLI                         
138600     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-TOT-VIKT                          
138700     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-TOT-VOLYM                         
138800                                                                          
138900     ADD +1 TO WS-ANTAL-ORDER                                             
139000     .                                                                    
139100     EJECT                                                                
139200                                                                          
139300 Z-STAENG-PRINTER SECTION.                                                
139400                                                                          
139500     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
139600                          PRT-CLOSE                                       
139700                          WS-PRT1                                         
139800                          ALT-PCB                                         
139900                          DUMMY-AREA                                      
140000                          DUMMY-AREA                                      
140100                                                                          
140200     IF MID-IDTRPTNR > 0 AND                                              
140300        MID-KDSVAR = 'F'                                                  
140400        CALL W006PRC1 USING PRT-VCOM                                      
140500                            PRT-CLOSE                                     
140600                            WS-PRT2                                       
140700                            ALT-PCB                                       
140800                            PRC1-W006PRVC                                 
140900     END-IF                                                               
141000     .                                                                    
141100     EJECT                                                                
141200 ZA-STAENG-PRINTER SECTION.                                               
141300                                                                          
141400     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
141500                          PRT-CLOSE                                       
141600                          WS-PRT1                                         
141700                          ALT-PCB                                         
141800                          DUMMY-AREA                                      
141900                          DUMMY-AREA                                      
142000     .                                                                    
142100     EJECT                                                                
142200                                                                          
142300 S01-SKRIV-HUVRUB-STAMPA SECTION.                                         
142400                                                                          
142500     MOVE +0 TO WS-RAD-RAKNARE1                                           
142600     ADD  +1 TO WS-SID-RAKNARE1                                           
142700                                                                          
142800     MOVE SPACE TO LIST-RAD                                               
142900     MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                  
143000                                                                          
143100     MOVE WS-SID-RAKNARE1 TO L1-STAMPA-RUB1-SIDNR                         
143200     MOVE L1-STAMPA-RUB1  TO LIST-RAD                                     
143300                                                                          
143400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
143500                         ALT-PCB PRT-RADSKIP                              
143600                         LIST-RAD                                         
143700     MOVE SPACE TO LIST-RAD                                               
143800     MOVE L1-STAMPA-RUB2  TO LIST-RAD                                     
143900     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
144000                                                                          
144100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
144200                         ALT-PCB PRT-RADSKIP                              
144300                         LIST-RAD                                         
144400                                                                          
144500     MOVE SPACE TO LIST-RAD                                               
144600     MOVE L1-STAMPA-RUB3  TO LIST-RAD                                     
144700                                                                          
144800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
144900                         ALT-PCB PRT-RADSKIP                              
145000                         LIST-RAD                                         
145100     ADD +5 TO WS-RAD-RAKNARE1                                            
145200     .                                                                    
145300     EJECT                                                                
145400                                                                          
145500 S03-SKRIV-KUND-RAD-STAMPA SECTION.                                       
145600                                                                          
145700     IF WS-RAD-RAKNARE1 > +38                                             
145800       PERFORM S01-SKRIV-HUVRUB-STAMPA                                    
145900     END-IF                                                               
146000                                                                          
146100     MOVE SPACE TO LIST-RAD                                               
146200     MOVE L1-STAMPA-RUB4  TO LIST-RAD                                     
146300     MOVE PRT-AFTER-2 TO PRT-RADSKIP                                      
146400                                                                          
146500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
146600                         ALT-PCB PRT-RADSKIP                              
146700                         LIST-RAD                                         
146800     ADD +2 TO WS-RAD-RAKNARE1                                            
146900                                                                          
147000     MOVE SPACE TO LIST-RAD                                               
147100     MOVE L1-STAMPA-RUB5  TO LIST-RAD                                     
147200                                                                          
147300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
147400                         ALT-PCB PRT-RADSKIP                              
147500                         LIST-RAD                                         
147600                                                                          
147700     ADD +2 TO WS-RAD-RAKNARE1                                            
147800     .                                                                    
147900     EJECT                                                                
148000                                                                          
148100 S04-SKRIV-RADER-STAMPA SECTION.                                          
148200                                                                          
148300     IF WS-RAD-RAKNARE1 > +38                                             
148400       PERFORM S01-SKRIV-HUVRUB-STAMPA                                    
148500     END-IF                                                               
148600                                                                          
148700     MOVE SPACE TO LIST-RAD                                               
148800     MOVE L1-STAMPA-RADER TO LIST-RAD                                     
148900                                                                          
149000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
149100                         ALT-PCB PRT-RADSKIP                              
149200                         LIST-RAD                                         
149300                                                                          
149400     MOVE PRT-AFTER-1 TO PRT-RADSKIP                                      
149500     ADD +1 TO WS-RAD-RAKNARE1                                            
149600                                                                          
149700     .                                                                    
149800     EJECT                                                                
149900                                                                          
150000 S05-SKRIV-TOTKUND-STAMPA SECTION.                                        
150100                                                                          
150200     IF WS-RAD-RAKNARE1 > +38                                             
150300       PERFORM S01-SKRIV-HUVRUB-STAMPA                                    
150400     END-IF                                                               
150500                                                                          
150600     MOVE SPACE TO LIST-RAD                                               
150700     MOVE L1-STAMPA-TOTAL-KUND TO LIST-RAD                                
150800     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
150900                                                                          
151000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
151100                         ALT-PCB PRT-RADSKIP                              
151200                         LIST-RAD                                         
151300                                                                          
151400     ADD +2 TO WS-RAD-RAKNARE1                                            
151500                                                                          
151600     .                                                                    
151700     EJECT                                                                
151800                                                                          
151900 S06-SKRIV-TOTAL-STAMPA SECTION.                                          
152000                                                                          
152100*    IF WS-RAD-RAKNARE1 > +40                                             
152200*      PERFORM S01-SKRIV-HUVRUB-STAMPA                                    
152300*    END-IF                                                               
152400                                                                          
152500     MOVE SPACE TO LIST-RAD                                               
152600     MOVE L1-HELA-STAMPA-RAD1 TO LIST-RAD                                 
152700     MOVE PRT-AFTER-2 TO PRT-RADSKIP                                      
152800                                                                          
152900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
153000                         ALT-PCB PRT-RADSKIP                              
153100                         LIST-RAD                                         
153200                                                                          
153300     MOVE PRT-AFTER-1 TO PRT-RADSKIP                                      
153400     MOVE SPACE TO LIST-RAD                                               
153500     MOVE L1-HELA-STAMPA-RAD2 TO LIST-RAD                                 
153600                                                                          
153700     ADD +2 TO WS-RAD-RAKNARE1                                            
153800                                                                          
153900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
154000                         ALT-PCB PRT-RADSKIP                              
154100                         LIST-RAD                                         
154200     .                                                                    
154300     EJECT                                                                
154400 S10-HAEMTA-KUND-UPPGIFT SECTION.                                         
154500                                                                          
154600     MOVE BOLLA-4494-IDDISTR  TO W-IDDISTR-WDB2                           
154700     MOVE BOLLA-4494-IDKUNDNR TO W-IDKUNDNR-WDB2                          
154800                                                                          
154900     PERFORM IMS-GU-WDB201                                                
155000     IF SEGMENT-FINNS                                                     
155100       MOVE GMT-BEGMT         TO L1-STAMPA-RUB4-BEGODSM                   
155200                                 LISTA-TOT-RUB4-BEGODSM                   
155300                                 LISTA2-BEGODSM-1                         
155400                                 L3-RUB4-TOT-BEGODSM                      
155500       MOVE GMT-ADGMT         TO L1-STAMPA-RUB4-ADGODSMK                  
155600                                 LISTA-TOT-RUB4-ADGODSMK                  
155700                                 L3-RUB4-TOT-ADGODSMK                     
155800       MOVE GMT-IDKUNDNR      TO LISTA-TOT-RUB4-KUNDNR                    
155900                                 L1-STAMPA-IDKUNDNR                       
156000                                 WS-IDKUNDNR                              
156100                                                                          
156200       IF GMT-FLLDCKND = JA                                               
156300         MOVE SPACE                    TO L1-STAMPA-TEXT2                 
156400                                          LISTA1-RAD-TEXT2                
156500                                          LISTA3-RAD-TEXT2                
156600       ELSE                                                               
156700         IF GMT-FLCOD = JA                                                
156800           MOVE 'SPEDIZIONE CONTRASSEGNO' TO L1-STAMPA-TEXT2              
156900                                            LISTA1-RAD-TEXT2              
157000                                            LISTA3-RAD-TEXT2              
157100         ELSE                                                             
157200           MOVE SPACE                  TO L1-STAMPA-TEXT2                 
157300                                            LISTA1-RAD-TEXT2              
157400                                            LISTA3-RAD-TEXT2              
157500         END-IF                                                           
157600       END-IF                                                             
157700     ELSE                                                                 
157800       MOVE 'CUSTOMER MISSING' TO L1-STAMPA-RUB4-BEGODSM                  
157900     END-IF                                                               
158000                                                                          
158100     ADD +1 TO WS-ANTAL-KUND                                              
158200     .                                                                    
158300     EJECT                                                                
158400                                                                          
158500 S11-SKRIV-RUB-LISTA1 SECTION.                                            
158600                                                                          
158700     MOVE +0 TO WS-RAD-RAKNARE1                                           
158800     ADD  +1 TO WS-SID-RAKNARE1                                           
158900                                                                          
159000     MOVE SPACE TO LIST-RAD                                               
159100     MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                  
159200                                                                          
159300     MOVE WS-SID-RAKNARE1 TO LISTA-TOT-SIDNR                              
159400     MOVE LISTA1-FINALE-RUB1 TO LIST-RAD                                  
159500                                                                          
159600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
159700                         ALT-PCB PRT-RADSKIP                              
159800                         LIST-RAD                                         
159900                                                                          
160000     MOVE SPACE TO LIST-RAD                                               
160100     MOVE LISTA1-FINALE-RUB2 TO LIST-RAD                                  
160200     MOVE PRT-AFTER-2 TO PRT-RADSKIP                                      
160300                                                                          
160400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
160500                         ALT-PCB PRT-RADSKIP                              
160600                         LIST-RAD                                         
160700                                                                          
160800     MOVE SPACE TO LIST-RAD                                               
160900     MOVE LISTA1-FINALE-RUB3 TO LIST-RAD                                  
161000                                                                          
161100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
161200                         ALT-PCB PRT-RADSKIP                              
161300                         LIST-RAD                                         
161400     ADD +5 TO WS-RAD-RAKNARE1                                            
161500     .                                                                    
161600     EJECT                                                                
161700                                                                          
161800 S13-SKRIV-KUND-RAD-LISTA1 SECTION.                                       
161900                                                                          
162000     IF WS-RAD-RAKNARE1 > +38                                             
162100       PERFORM S11-SKRIV-RUB-LISTA1                                       
162200     END-IF                                                               
162300                                                                          
162400     MOVE SPACE TO LIST-RAD                                               
162500     MOVE LISTA1-FINALE-RUB4 TO LIST-RAD                                  
162600     MOVE PRT-AFTER-2   TO PRT-RADSKIP                                    
162700                                                                          
162800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
162900                         ALT-PCB PRT-RADSKIP                              
163000                         LIST-RAD                                         
163100     ADD +2 TO WS-RAD-RAKNARE1                                            
163200                                                                          
163300     MOVE SPACE TO LIST-RAD                                               
163400     MOVE LISTA1-FINALE-RAD-RUB TO LIST-RAD                               
163500                                                                          
163600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
163700                         ALT-PCB PRT-RADSKIP                              
163800                         LIST-RAD                                         
163900     ADD +2 TO WS-RAD-RAKNARE1                                            
164000     .                                                                    
164100     EJECT                                                                
164200                                                                          
164300 S14-SKRIV-RADER-LISTA1 SECTION.                                          
164400                                                                          
164500     IF WS-RAD-RAKNARE1 > +38                                             
164600       PERFORM S11-SKRIV-RUB-LISTA1                                       
164700     END-IF                                                               
164800                                                                          
164900     MOVE SPACE TO LIST-RAD                                               
165000     MOVE LISTA1-FINALE-RADER TO LIST-RAD                                 
165100                                                                          
165200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
165300                         ALT-PCB PRT-RADSKIP                              
165400                         LIST-RAD                                         
165500                                                                          
165600     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
165700     ADD +1 TO WS-RAD-RAKNARE1                                            
165800                                                                          
165900     .                                                                    
166000     EJECT                                                                
166100                                                                          
166200 S15-SKRIV-TOTKUND-LISTA1 SECTION.                                        
166300                                                                          
166400     IF WS-RAD-RAKNARE1 > +38                                             
166500       PERFORM S11-SKRIV-RUB-LISTA1                                       
166600     END-IF                                                               
166700                                                                          
166800     MOVE SPACE TO LIST-RAD                                               
166900     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
167000     MOVE LISTA1-FINALE-TOT-KUND TO LIST-RAD                              
167100                                                                          
167200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
167300                         ALT-PCB PRT-RADSKIP                              
167400                         LIST-RAD                                         
167500                                                                          
167600     ADD +2 TO WS-RAD-RAKNARE1                                            
167700                                                                          
167800     .                                                                    
167900     EJECT                                                                
168000                                                                          
168100                                                                          
168200 S16-SKRIV-TOTAL-LISTA1 SECTION.                                          
168300                                                                          
168400*    IF WS-RAD-RAKNARE1 > +42                                             
168500*      PERFORM S11-SKRIV-RUB-LISTA1                                       
168600*    END-IF                                                               
168700                                                                          
168800     MOVE SPACE TO LIST-RAD                                               
168900     MOVE LISTA1-FINALE-HELA-RAD1 TO LIST-RAD                             
169000     MOVE PRT-AFTER-2    TO PRT-RADSKIP                                   
169100                                                                          
169200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
169300                         ALT-PCB PRT-RADSKIP                              
169400                         LIST-RAD                                         
169500                                                                          
169600     ADD +2 TO WS-RAD-RAKNARE1                                            
169700     MOVE PRT-AFTER-1    TO PRT-RADSKIP                                   
169800     MOVE SPACE TO LIST-RAD                                               
169900     MOVE LISTA1-FINALE-HELA-RAD2 TO LIST-RAD                             
170000                                                                          
170100                                                                          
170200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
170300                         ALT-PCB PRT-RADSKIP                              
170400                         LIST-RAD                                         
170500     ADD +1 TO WS-RAD-RAKNARE1                                            
170600     .                                                                    
170700     EJECT                                                                
170800 S21-SKRIV-RUB-LISTA2 SECTION.                                            
170900                                                                          
171000     MOVE +0 TO WS-RAD-RAKNARE2                                           
171100     ADD  +1 TO WS-SID-RAKNARE2                                           
171200                                                                          
171300     MOVE SPACE TO LIST-RAD                                               
171400                                                                          
171500     MOVE WS-SID-RAKNARE2 TO LISTA-TOT-SIDNR                              
171600     MOVE LISTA1-FINALE-RUB1 TO LIST-RAD                                  
171700     MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                  
171800                                                                          
171900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
172000                         ALT-PCB PRT-RADSKIP                              
172100                         LIST-RAD                                         
172200                                                                          
172300     MOVE SPACE TO LIST-RAD                                               
172400     MOVE LISTA1-FINALE-RUB3 TO LIST-RAD                                  
172500     MOVE PRT-AFTER-2 TO PRT-RADSKIP                                      
172600                                                                          
172700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
172800                         ALT-PCB PRT-RADSKIP                              
172900                         LIST-RAD                                         
173000                                                                          
173100     MOVE SPACE TO LIST-RAD                                               
173200     MOVE LISTA2-FINALE-RUB1 TO LIST-RAD                                  
173300                                                                          
173400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
173500                         ALT-PCB PRT-RADSKIP                              
173600                         LIST-RAD                                         
173700                                                                          
173800     MOVE SPACE TO LIST-RAD                                               
173900     MOVE LISTA2-FINALE-RUB2 TO LIST-RAD                                  
174000     MOVE PRT-AFTER-1 TO PRT-RADSKIP                                      
174100                                                                          
174200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
174300                         ALT-PCB PRT-RADSKIP                              
174400                         LIST-RAD                                         
174500     ADD +5 TO WS-RAD-RAKNARE2                                            
174600     .                                                                    
174700     EJECT                                                                
174800 S24-SKRIV-RADER-LISTA2 SECTION.                                          
174900                                                                          
175000     IF WS-RAD-RAKNARE2 > +42                                             
175100       PERFORM S31-SKRIV-RUB-LISTA3                                       
175200     END-IF                                                               
175300                                                                          
175400     MOVE SPACE TO LIST-RAD                                               
175500     MOVE LISTA2-FINALE-RADER TO LIST-RAD                                 
175600                                                                          
175700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
175800                         ALT-PCB PRT-RADSKIP                              
175900                         LIST-RAD                                         
176000                                                                          
176100                                                                          
176200     ADD +1 TO WS-RAD-RAKNARE2                                            
176300                                                                          
176400     .                                                                    
176500     EJECT                                                                
176600                                                                          
176700                                                                          
176800 S31-SKRIV-RUB-LISTA3 SECTION.                                            
176900                                                                          
177000     MOVE +0 TO WS-RAD-RAKNARE3                                           
177100     ADD  +1 TO WS-SID-RAKNARE3                                           
177200                                                                          
177300     MOVE SPACE TO LIST-RAD                                               
177400                                                                          
177500     MOVE WS-SID-RAKNARE3 TO LISTA-TOT-SIDNR                              
177600     MOVE LISTA1-FINALE-RUB1 TO LIST-RAD                                  
177700     MOVE PRT-NYSIDA-RAD1    TO PRT-RADSKIP                               
177800                                                                          
177900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
178000                         ALT-PCB PRT-RADSKIP                              
178100                         LIST-RAD                                         
178200                                                                          
178300     MOVE SPACE TO LIST-RAD                                               
178400     MOVE LISTA1-FINALE-RUB2 TO LIST-RAD                                  
178500     MOVE PRT-AFTER-2 TO PRT-RADSKIP                                      
178600                                                                          
178700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
178800                         ALT-PCB PRT-RADSKIP                              
178900                         LIST-RAD                                         
179000                                                                          
179100     MOVE SPACE TO LIST-RAD                                               
179200     MOVE LISTA1-FINALE-RUB3 TO LIST-RAD                                  
179300                                                                          
179400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
179500                         ALT-PCB PRT-RADSKIP                              
179600                         LIST-RAD                                         
179700     ADD +5 TO WS-RAD-RAKNARE3                                            
179800     .                                                                    
179900     EJECT                                                                
180000                                                                          
180100 S33-SKRIV-KUND-RAD-LISTA3 SECTION.                                       
180200                                                                          
180300     IF WS-RAD-RAKNARE3 > +38                                             
180400       PERFORM S31-SKRIV-RUB-LISTA3                                       
180500     END-IF                                                               
180600                                                                          
180700     MOVE SPACE TO LIST-RAD                                               
180800     MOVE LISTA1-FINALE-RUB4 TO LIST-RAD                                  
180900     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
181000                                                                          
181100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
181200                         ALT-PCB PRT-RADSKIP                              
181300                         LIST-RAD                                         
181400     ADD +2 TO WS-RAD-RAKNARE3                                            
181500                                                                          
181600     MOVE SPACE TO LIST-RAD                                               
181700     MOVE LISTA3-FINALE-RUB-RAD TO LIST-RAD                               
181800                                                                          
181900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
182000                         ALT-PCB PRT-RADSKIP                              
182100                         LIST-RAD                                         
182200     ADD +2 TO WS-RAD-RAKNARE3                                            
182300     .                                                                    
182400     EJECT                                                                
182500 S34-SKRIV-RADER-LISTA3  SECTION.                                         
182600                                                                          
182700     IF WS-RAD-RAKNARE3 > +38                                             
182800       PERFORM S31-SKRIV-RUB-LISTA3                                       
182900     END-IF                                                               
183000                                                                          
183100                                                                          
183200     MOVE SPACE TO LIST-RAD                                               
183300     MOVE LISTA3-FINALE-RADER TO LIST-RAD                                 
183400                                                                          
183500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
183600                         ALT-PCB PRT-RADSKIP                              
183700                         LIST-RAD                                         
183800                                                                          
183900     MOVE  PRT-AFTER-1 TO PRT-RADSKIP                                     
184000                                                                          
184100     ADD +1 TO WS-RAD-RAKNARE3                                            
184200                                                                          
184300     .                                                                    
184400     EJECT                                                                
184500                                                                          
184600 S35-SKRIV-TOTKUND-LISTA3 SECTION.                                        
184700                                                                          
184800     IF WS-RAD-RAKNARE3 > +38                                             
184900       PERFORM S31-SKRIV-RUB-LISTA3                                       
185000     END-IF                                                               
185100                                                                          
185200     MOVE SPACE TO LIST-RAD                                               
185300     MOVE LISTA3-FINALE-TOT-KUND TO LIST-RAD                              
185400     MOVE PRT-AFTER-2   TO PRT-RADSKIP                                    
185500                                                                          
185600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
185700                         ALT-PCB PRT-RADSKIP                              
185800                         LIST-RAD                                         
185900                                                                          
186000     ADD +2 TO WS-RAD-RAKNARE3                                            
186100                                                                          
186200     .                                                                    
186300     EJECT                                                                
186400 S36-SKRIV-TOTAL-LISTA3 SECTION.                                          
186500                                                                          
186600     IF WS-RAD-RAKNARE3 > +36                                             
186700       PERFORM S31-SKRIV-RUB-LISTA3                                       
186800     END-IF                                                               
186900                                                                          
187000     MOVE SPACE TO LIST-RAD                                               
187100     MOVE LISTA3-FINALE-HELA-RAD1 TO LIST-RAD                             
187200     MOVE PRT-AFTER-2    TO PRT-RADSKIP                                   
187300                                                                          
187400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
187500                         ALT-PCB PRT-RADSKIP                              
187600                         LIST-RAD                                         
187700                                                                          
187800     ADD +2 TO WS-RAD-RAKNARE3                                            
187900                                                                          
188000     MOVE PRT-AFTER-1    TO PRT-RADSKIP                                   
188100     MOVE SPACE TO LIST-RAD                                               
188200     MOVE LISTA3-FINALE-HELA-RAD2 TO LIST-RAD                             
188300                                                                          
188400                                                                          
188500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1                  
188600                         ALT-PCB PRT-RADSKIP                              
188700                         LIST-RAD                                         
188800     ADD +1 TO WS-RAD-RAKNARE3                                            
188900     .                                                                    
189000     EJECT                                                                
189100                                                                          
189200 S99-SKRIV-VCOM-FIL SECTION.                                              
189300                                                                          
189400     MOVE PRT-AFTER-1    TO PRT-RADSKIP                                   
189500     MOVE LISTRAD-VCOM TO LIST-RAD                                        
189600                          PRC1-DATA                                       
189700                                                                          
189800     CALL W006PRC1 USING PRT-VCOM PRT-WRITE WS-PRT2                       
189900                         ALT-PCB                                          
190000                         PRC1-W006PRVC                                    
190100     .                                                                    
190200     EJECT                                                                
190300* --- IMS SEKTIONER ---                                                   
190400     SKIP3                                                                
190500 IMS-GET-MSG SECTION.                                                     
190600                                                                          
190700     MOVE '  QC' TO GODK-STATUSKODER                                      
190800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
190900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
191000     PERFORM IMS-STATUSKONTROLL                                           
191100     .                                                                    
191200     SKIP3                                                                
191300 IMS-GU-WDB201 SECTION.                                                   
191400                                                                          
191500     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
191600          DELIMITED BY SIZE INTO SSA1                                     
191700     MOVE '  ' TO GODK-STATUSKODER                                        
191800     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA2 SSA1                     
191900     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
192000     PERFORM IMS-STATUSKONTROLL                                           
192100     .                                                                    
192200     EJECT                                                                
192300 IMS-GHU-WL4491 SECTION.                                                  
192400                                                                          
192500     STRING 'WL449101(WDGXKEY  =' W-WDGXKEY-X ')'                         
192600          DELIMITED BY SIZE INTO SSA1                                     
192700     MOVE '  ' TO GODK-STATUSKODER                                        
192800     CALL CBLTDLI USING GHU BOLLA-PCB DLI-IO-AREA SSA1                    
192900     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
193000     PERFORM IMS-STATUSKONTROLL                                           
193100     .                                                                    
193200     SKIP2                                                                
193300 IMS-GHNP-WL4494 SECTION.                                                 
193400                                                                          
193500     STRING 'WL449112(KY4494  >=' W-KY4494-MIN-X                          
193600                    '&KY4494  <=' W-KY4494-MAX-X                          
193700                    '&IDLBBET  =' W-IDLBBET-X ')'                         
193800          DELIMITED BY SIZE INTO SSA1                                     
193900     MOVE '  GE' TO GODK-STATUSKODER                                      
194000     CALL CBLTDLI USING GHNP BOLLA-PCB DLI-IO-AREA1 SSA1                  
194100     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
194200     PERFORM IMS-STATUSKONTROLL                                           
194300     .                                                                    
194400     SKIP2                                                                
194500 IMS-GHNP-WL4494-TRP SECTION.                                             
194600                                                                          
194700     STRING 'WL449112(KY4494  >=' W-KY4494-MIN-X                          
194800                    '&KY4494  <=' W-KY4494-MAX-X ')'                      
194900          DELIMITED BY SIZE INTO SSA1                                     
195000     MOVE '  GE' TO GODK-STATUSKODER                                      
195100     CALL CBLTDLI USING GHNP BOLLA-PCB DLI-IO-AREA1 SSA1                  
195200     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
195300     PERFORM IMS-STATUSKONTROLL                                           
195400     .                                                                    
195500     SKIP2                                                                
195600 IMS-GHNP-WL4494-TRP-FIRST SECTION.                                       
195700                                                                          
195800     STRING 'WL449112*F(KY4494  >=' W-KY4494-MIN-X                        
195900                      '&KY4494  <=' W-KY4494-MAX-X ')'                    
196000          DELIMITED BY SIZE INTO SSA1                                     
196100     MOVE '  GE' TO GODK-STATUSKODER                                      
196200     CALL CBLTDLI USING GHNP BOLLA-PCB DLI-IO-AREA1 SSA1                  
196300     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
196400     PERFORM IMS-STATUSKONTROLL                                           
196500     .                                                                    
196600     SKIP2                                                                
196700 IMS-GHNP-FIRST-WL4492 SECTION.                                           
196800                                                                          
196900     MOVE 'WL449111*F' TO SSA1                                            
197000     MOVE '  ' TO GODK-STATUSKODER                                        
197100     CALL CBLTDLI USING GHNP BOLLA-PCB DLI-IO-AREA SSA1                   
197200     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
197300     PERFORM IMS-STATUSKONTROLL                                           
197400     .                                                                    
197500     EJECT                                                                
197600 IMS-REPLACE-WL4492 SECTION.                                              
197700     SKIP2                                                                
197800     MOVE '  ' TO GODK-STATUSKODER                                        
197900     CALL CBLTDLI USING REPL BOLLA-PCB DLI-IO-AREA                        
198000     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
198100     PERFORM IMS-STATUSKONTROLL                                           
198200     .                                                                    
198300     SKIP2                                                                
198400 IMS-REPLACE-WL4494 SECTION.                                              
198500     SKIP2                                                                
198600     MOVE '  ' TO GODK-STATUSKODER                                        
198700     CALL CBLTDLI USING REPL BOLLA-PCB DLI-IO-AREA1                       
198800     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
198900     PERFORM IMS-STATUSKONTROLL                                           
199000     .                                                                    
199100     EJECT                                                                
199200                                                                          
199300 IMS-STATUSKONTROLL SECTION.                                              
199400                                                                          
199500     SET STATUS-IX TO 1                                                   
199600     SEARCH GODK-STATUS                                                   
199700       AT END                                                             
199800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
199900         DELIMITED BY SIZE INTO FELTEXT                                   
200000         CALL FELLOG                                                      
200100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
200200         CONTINUE                                                         
200300     END-SEARCH                                                           
200400     .                                                                    
