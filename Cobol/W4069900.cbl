000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4069900.                                                
000300 AUTHOR.         INGVAR SKJELBRED.                                        
000400 DATE-WRITTEN.   96/01/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610                                                                          
000700*    FUNKTION:                                                            
000800*        BAKGRUNDSTRANS STARTAS AV W40664 OCH SKRIVER                     
000900*        UT LISTAN LISTA DE EMBARQUE FÖR                                  
001000*              DC24 - SPANIEN                                             
001100*        ELLER DC26 ÖSTERRIKE                                             
001200*        ELLER DC61 Japan                                                 
001300*        ELLER DC62 Australien                                            
001400*                                                                         
001500*        PROGRAMMET LÄSER      WL4495 (WDR4)                              
001600*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T699                                              
002000*        MID:         W4I66401                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W4O66402                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W4069900'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000 77  SPRAK-IX                    PIC S9(9) VALUE +0 COMP SYNC.            
004100                                                                          
004200 77  RAD-INDX                    PIC S9(7)   VALUE ZERO COMP-3.           
004300 77  WS-RAD-RAKNARE              PIC 9(3)    VALUE ZERO.                  
004400 77  WS-SID-RAKNARE              PIC 9(3)    VALUE ZERO.                  
004500 77  TEXT-IX                     PIC 9(1)    VALUE ZERO.                  
004600                                                                          
004700 77  WS-IDTRPTNR                 PIC X(3)   VALUE SPACE.                  
004800                                                                          
004900 77  SPAR-IDDEALER               PIC 9(7)   VALUE ZERO.                   
005000                                                                          
005100 77  WS-IDLBBET                  PIC X(12)  VALUE SPACE.                  
005200                                                                          
005300 77  WS-VKORDBTO-TOT             PIC S9(6)V9(1) COMP-3.                   
005400                                                                          
005500 77  WS-VLORDBTO-TOT             PIC S9(4)V9(3) COMP-3.                   
005600                                                                          
005700 77  WS-KVKOLLI-TOT              PIC S9(5)      COMP-3.                   
005800                                                                          
005900 77  WS-FLFARLIG                 PIC X(1)    VALUE SPACE.                 
006000                                                                          
006100 01  FILLER                      PIC X(16)   VALUE 'DATUM O TID'.         
006200                                                                          
006300 01  DAGENS-DATUM                PIC 9(6).                                
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-AA               PIC 9(2).                                
006600     03  DAGENS-MM               PIC 9(2).                                
006700     03  DAGENS-DD               PIC 9(2).                                
006800                                                                          
006900 01  WS-DATUM.                                                            
007000     03  WS-SEKEL                PIC 9(2).                                
007100     03  WS-AAR                  PIC 9(2).                                
007200     03  WS-MAN                  PIC 9(2).                                
007300     03  WS-DAG                  PIC 9(2).                                
007400                                                                          
007500 01  DAGENS-TID-LOKAL            PIC 9(4).                                
007600 01  FILLER REDEFINES DAGENS-TID-LOKAL.                                   
007700     03  DAGENS-HH-LOK           PIC 9(2).                                
007800     03  DAGENS-MIN-LOK          PIC 9(2).                                
007900                                                                          
008000 01  WS-LIST-AREA.                                                        
008100     03  WS-DUMMY                PIC X.                                   
008200     03  WS-IDPRTLST             PIC X(8) VALUE SPACE.                    
008300     03  WS-LISTRAD.                                                      
008400       05  FILLER                PIC X(4) VALUE SPACE.                    
008500       05  LISTRAD               PIC X(78) VALUE SPACE.                   
008600                                                                          
008700 77  WS-ADRESS                   PIC X      VALUE SPACE.                  
008800*      --- VALID IDDC CODES                                               
008900*                                                                         
009000*01    -COPY WWDC99                                                       
009100       EJECT                                                              
009200                                                                          
009300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009400                                                                          
009500                                                                          
009600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009700     88  NYCKLAR-OK                          VALUE 'J'.                   
009800     88  NYCKLAR-FEL                         VALUE 'N'.                   
009900                                                                          
010000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010100     88  EGEN-MID                            VALUE '4699'.                
010200     88  GODK-MID                            VALUE '4691' '4692'          
010300                                                   '4693' '4694'          
010400                                                   '4695' '4696'          
010500                                                   '4697' '4698'          
010600                                                   '4699'.                
010700     88  HELP-MID                            VALUE '0551'.                
010800     EJECT                                                                
010900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011000 01  GENERELLA-SUBPROGRAM.                                                
011100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011600     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011900*01 -COPY WMEDAREA                                                        
012000     SKIP3                                                                
012100 01  MESSAGE-CODES.                                                       
012200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012300     03  START-PRINT             PIC X(3)    VALUE '118'.                 
012400     03  NO-PRINTING             PIC X(3)    VALUE '167'.                 
012500     EJECT                                                                
012600*01  -COPY W006PRAR                                                       
012700     EJECT                                                                
012800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013100     SKIP3                                                                
013200*01 -COPY WMSGINIT                                                        
013300     SKIP3                                                                
013400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013500 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
013600*01 -COPY WDATAREA                                                        
013700     EJECT                                                                
013800*                                                                         
013900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014200     SKIP3                                                                
014300*01  MID -COPY W4I66401                                                   
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014600     SKIP3                                                                
014700*01  -COPY WMSGAREA                                                       
014800     EJECT                                                                
014900     03  MOD REDEFINES MSG-AREA.                                          
015000*      05  -COPY W4O66402                                                 
015100     EJECT                                                                
015200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015300     SKIP3                                                                
015400*01  -COPY WMFSAREA                                                       
015500     EJECT                                                                
015600********  LIST RUBRIKER OCH RADER (42 RADER) *********************        
015700 01  FILLER                    PIC X(16) VALUE 'LIST RUBRIKER'.           
015800***                                                                       
015900 01  WS-LISTRAD.                                                          
016000     03  LIST-RAD              PIC X(132).                                
016100                                                                          
016200                                                                          
016300********* LISTHUVUDET PÅ LISTAN *******************                       
016400                                                                          
016500 01  RUB1.                                                                
016600     03 RUB1-ES.                                                          
016700       05 FILLER                   PIC X(50)  VALUE SPACE.                
016800       05 FILLER                   PIC X(21)  VALUE                       
016900       'LISTA DE EMBARQUE    '.                                           
017000       05 FILLER                   PIC X(31)  VALUE SPACE.                
017100       05 FILLER                   PIC X(8)   VALUE 'PAGINA: '.           
017200       05 FILLER                   PIC Z(2)9.                             
017300                                                                          
017400     03 RUB1-AT.                                                          
017500       05 FILLER                   PIC X(50)  VALUE SPACE.                
017600       05 FILLER                   PIC X(21)  VALUE                       
017700       'LOADE LISTE          '.                                           
017800       05 FILLER                   PIC X(31)  VALUE SPACE.                
017900       05 FILLER                   PIC X(8)   VALUE 'Zeit:   '.           
018000       05 FILLER                   PIC Z(2)9.                             
018100                                                                          
018200     03 RUB1-JP.                                                          
018300       05 FILLER                   PIC X(50)  VALUE SPACE.                
018400       05 FILLER                   PIC X(21)  VALUE                       
018500       'LOADING REPORT       '.                                           
018600       05 FILLER                   PIC X(31)  VALUE SPACE.                
018700       05 FILLER                   PIC X(8)   VALUE '        '.           
018800       05 FILLER                   PIC Z(2)9.                             
018900                                                                          
019000     03 RUB1-AU.                                                          
019100       05 FILLER                   PIC X(50)  VALUE SPACE.                
019200       05 FILLER                   PIC X(21)  VALUE                       
019300       'LOADING REPORT       '.                                           
019400       05 FILLER                   PIC X(31)  VALUE SPACE.                
019500       05 FILLER                   PIC X(8)   VALUE '        '.           
019600       05 FILLER                   PIC Z(2)9.                             
019700                                                                          
019800     03 RUB1-SE.                                                          
019900       05 FILLER                   PIC X(50)  VALUE SPACE.                
020000       05 FILLER                   PIC X(21)  VALUE                       
020100       'LOADING REPORT       '.                                           
020200       05 FILLER                   PIC X(31)  VALUE SPACE.                
020300       05 FILLER                   PIC X(8)   VALUE '        '.           
020400       05 FILLER                   PIC Z(2)9.                             
020500                                                                          
020600 01  FILLER REDEFINES RUB1.                                               
020700     03 RUB-1 OCCURS 5.                                                   
020800       05 FILLER                   PIC X(50).                             
020900       05 L1-DE-EMBARQUE-RUB1-DEL1 PIC X(21).                             
021000       05 FILLER                   PIC X(31).                             
021100       05 L1-EMBARQUE-RUB1-SIDA    PIC X(8).                              
021200       05 L1-EMBARQUE-RUB1-SIDNR   PIC Z(2)9.                             
021300                                                                          
021400 01  RUB2.                                                                
021500     03 RUB2-ES.                                                          
021600       05 FILLER                   PIC X(3)  VALUE SPACE.                 
021700       05 FILLER                   PIC X(18)  VALUE                       
021800             'VOLVO ESPANA, S.A.'.                                        
021900     03 RUB2-AT.                                                          
022000       05 FILLER                   PIC X(3)  VALUE SPACE.                 
022100       05 FILLER                   PIC X(18)  VALUE                       
022200             'Volvo Parts Vienna'.                                        
022300                                                                          
022400     03 RUB2-JP.                                                          
022500       05 FILLER                   PIC X(3)  VALUE SPACE.                 
022600       05 FILLER                   PIC X(18)  VALUE                       
022700             '                  '.                                        
022800                                                                          
022900     03 RUB2-AU.                                                          
023000       05 FILLER                   PIC X(3)  VALUE SPACE.                 
023100       05 FILLER                   PIC X(18)  VALUE                       
023200             '                  '.                                        
023300                                                                          
023400     03 RUB2-SE.                                                          
023500       05 FILLER                   PIC X(3)  VALUE SPACE.                 
023600       05 FILLER                   PIC X(18)  VALUE                       
023700             '                  '.                                        
023800                                                                          
023900 01 FILLER REDEFINES RUB2.                                                
024000     03 RUB-2 OCCURS 5.                                                   
024100       05 FILLER                   PIC X(3).                              
024200       05 L1-RUB2-RUB              PIC X(18).                             
024300                                                                          
024400 01 RUB3.                                                                 
024500     03 RUB3-ES.                                                          
024600       05 FILLER                   PIC X(3)   VALUE SPACE.                
024700       05 FILLER                   PIC X(29)  VALUE                       
024800              'CENTRO DISTRIBUCION REPUESTOS'.                            
024900       05 FILLER                   PIC X(11) VALUE SPACE.                 
025000       05 FILLER                   PIC X(12)  VALUE                       
025100              'FECHA/HORA:'.                                              
025200       05 FILLER                   PIC X(2)  VALUE SPACE.                 
025300       05 FILLER                   PIC X(2)  VALUE SPACE.                 
025400       05 FILLER                   PIC X(1)  VALUE '/'.                   
025500       05 FILLER                   PIC X(2)  VALUE SPACE.                 
025600       05 FILLER                   PIC X(1)  VALUE '/'.                   
025700       05 FILLER                   PIC X(2)  VALUE SPACE.                 
025800       05 FILLER                   PIC X(2)  VALUE SPACE.                 
025900       05 FILLER                   PIC X(2)  VALUE SPACE.                 
026000       05 FILLER                   PIC X(1)  VALUE ':'.                   
026100       05 FILLER                   PIC X(2)  VALUE SPACE.                 
026200                                                                          
026300     03 RUB3-AT.                                                          
026400       05 FILLER                   PIC X(3)   VALUE SPACE.                
026500       05 FILLER                   PIC X(29)  VALUE                       
026600              'Industrie Park Eco Plus      '.                            
026700       05 FILLER                   PIC X(11) VALUE SPACE.                 
026800       05 FILLER                   PIC X(12)  VALUE                       
026900              'Datem/Zeil:'.                                              
027000       05 FILLER                   PIC X(2)  VALUE SPACE.                 
027100       05 FILLER                   PIC X(2)  VALUE SPACE.                 
027200       05 FILLER                   PIC X(1)  VALUE '/'.                   
027300       05 FILLER                   PIC X(2)  VALUE SPACE.                 
027400       05 FILLER                   PIC X(1)  VALUE '/'.                   
027500       05 FILLER                   PIC X(2)  VALUE SPACE.                 
027600       05 FILLER                   PIC X(2)  VALUE SPACE.                 
027700       05 FILLER                   PIC X(2)  VALUE SPACE.                 
027800       05 FILLER                   PIC X(1)  VALUE ':'.                   
027900       05 FILLER                   PIC X(2)  VALUE SPACE.                 
028000                                                                          
028100     03 RUB3-JP.                                                          
028200       05 FILLER                   PIC X(3)   VALUE SPACE.                
028300       05 FILLER                   PIC X(29)  VALUE                       
028400              '1-5-10, Higashihamna,        '.                            
028500       05 FILLER                   PIC X(11) VALUE SPACE.                 
028600       05 FILLER                   PIC X(12)  VALUE                       
028700              'Date /Time:'.                                              
028800       05 FILLER                   PIC X(2)  VALUE SPACE.                 
028900       05 FILLER                   PIC X(2)  VALUE SPACE.                 
029000       05 FILLER                   PIC X(1)  VALUE '/'.                   
029100       05 FILLER                   PIC X(2)  VALUE SPACE.                 
029200       05 FILLER                   PIC X(1)  VALUE '/'.                   
029300       05 FILLER                   PIC X(2)  VALUE SPACE.                 
029400       05 FILLER                   PIC X(2)  VALUE SPACE.                 
029500       05 FILLER                   PIC X(2)  VALUE SPACE.                 
029600       05 FILLER                   PIC X(1)  VALUE ':'.                   
029700       05 FILLER                   PIC X(2)  VALUE SPACE.                 
029800                                                                          
029900     03 RUB3-AU.                                                          
030000       05 FILLER                   PIC X(3)   VALUE SPACE.                
030100       05 FILLER                   PIC X(29)  VALUE                       
030200              'Lot 12. Airds Road           '.                            
030300       05 FILLER                   PIC X(11) VALUE SPACE.                 
030400       05 FILLER                   PIC X(12)  VALUE                       
030500              'Date /Time:'.                                              
030600       05 FILLER                   PIC X(2)  VALUE SPACE.                 
030700       05 FILLER                   PIC X(2)  VALUE SPACE.                 
030800       05 FILLER                   PIC X(1)  VALUE '/'.                   
030900       05 FILLER                   PIC X(2)  VALUE SPACE.                 
031000       05 FILLER                   PIC X(1)  VALUE '/'.                   
031100       05 FILLER                   PIC X(2)  VALUE SPACE.                 
031200       05 FILLER                   PIC X(2)  VALUE SPACE.                 
031300       05 FILLER                   PIC X(2)  VALUE SPACE.                 
031400       05 FILLER                   PIC X(1)  VALUE ':'.                   
031500       05 FILLER                   PIC X(2)  VALUE SPACE.                 
031600                                                                          
031700     03 RUB3-SE.                                                          
031800       05 FILLER                   PIC X(3)   VALUE SPACE.                
031900       05 FILLER                   PIC X(29)  VALUE                       
032000              '                             '.                            
032100       05 FILLER                   PIC X(11) VALUE SPACE.                 
032200       05 FILLER                   PIC X(12)  VALUE                       
032300              'Date /Time:'.                                              
032400       05 FILLER                   PIC X(2)  VALUE SPACE.                 
032500       05 FILLER                   PIC X(2)  VALUE SPACE.                 
032600       05 FILLER                   PIC X(1)  VALUE '/'.                   
032700       05 FILLER                   PIC X(2)  VALUE SPACE.                 
032800       05 FILLER                   PIC X(1)  VALUE '/'.                   
032900       05 FILLER                   PIC X(2)  VALUE SPACE.                 
033000       05 FILLER                   PIC X(2)  VALUE SPACE.                 
033100       05 FILLER                   PIC X(2)  VALUE SPACE.                 
033200       05 FILLER                   PIC X(1)  VALUE ':'.                   
033300       05 FILLER                   PIC X(2)  VALUE SPACE.                 
033400                                                                          
033500 01 FILLER REDEFINES RUB3.                                                
033600     03 RUB-3 OCCURS 5.                                                   
033700       05 FILLER                   PIC X(3).                              
033800       05 L1-RUB3                  PIC X(29).                             
033900       05 FILLER                   PIC X(11).                             
034000       05 FILLER                   PIC X(12).                             
034100       05 L1-RUB3-SEKEL            PIC X(2).                              
034200       05 LI-RUB3-YEAR             PIC X(2).                              
034300       05 FILLER                   PIC X(1).                              
034400       05 L1-RUB3-MANAD            PIC X(2).                              
034500       05 FILLER                   PIC X(1).                              
034600       05 L1-RUB3-DAG              PIC X(2).                              
034700       05 FILLER                   PIC X(2).                              
034800       05 L1-RUB3-TIMME            PIC X(2).                              
034900       05 FILLER                   PIC X(1).                              
035000       05 L1-RUB3-MINUT            PIC X(2).                              
035100                                                                          
035200 01 RUB4.                                                                 
035300     03 RUB4-ES.                                                          
035400       05 FILLER                   PIC X(3)   VALUE SPACE.                
035500       05 FILLER                   PIC X(26)  VALUE                       
035600               'AVENIDA DE LAINDUSTRIA ,16'.                              
035700                                                                          
035800     03 RUB4-AT.                                                          
035900       05 FILLER                   PIC X(3)   VALUE SPACE.                
036000       05 FILLER                   PIC X(26)  VALUE                       
036100               '1. Strasse                '.                              
036200                                                                          
036300     03 RUB4-JP.                                                          
036400       05 FILLER                   PIC X(3)   VALUE SPACE.                
036500       05 FILLER                   PIC X(26)  VALUE                       
036600               'Tobishima-mura, Ama-gun,  '.                              
036700                                                                          
036800     03 RUB4-au.                                                          
036900       05 FILLER                   PIC X(3)   VALUE SPACE.                
037000       05 FILLER                   PIC X(26)  VALUE                       
037100               'Minto NSW 2566            '.                              
037200                                                                          
037300     03 RUB4-SE.                                                          
037400       05 FILLER                   PIC X(3)   VALUE SPACE.                
037500       05 FILLER                   PIC X(26)  VALUE                       
037600               '                          '.                              
037700                                                                          
037800 01 FILLER REDEFINES RUB4.                                                
037900     03 RUB-4 OCCURS 5.                                                   
038000       05 FILLER                   PIC X(3).                              
038100       05 L1-RUB4                  PIC X(26).                             
038200                                                                          
038300 01 RUB5.                                                                 
038400     03 RUB5-ES.                                                          
038500       05 FILLER                   PIC X(3)   VALUE SPACE.                
038600       05 FILLER                   PIC X(25)  VALUE                       
038700              '19004 POLIGONO HENARES   '.                                
038800                                                                          
038900     03 RUB5-AT.                                                          
039000       05 FILLER                   PIC X(3)   VALUE SPACE.                
039100       05 FILLER                   PIC X(25)  VALUE                       
039200              'A-2560 Bruk an det Leitha'.                                
039300                                                                          
039400     03 RUB5-JP.                                                          
039500       05 FILLER                   PIC X(3)   VALUE SPACE.                
039600       05 FILLER                   PIC X(25)  VALUE                       
039700              'Aichi 490-1446, Japan    '.                                
039800                                                                          
039900     03 RUB5-AU.                                                          
040000       05 FILLER                   PIC X(3)   VALUE SPACE.                
040100       05 FILLER                   PIC X(25)  VALUE                       
040200              'Australia                '.                                
040300                                                                          
040400     03 RUB5-SE.                                                          
040500       05 FILLER                   PIC X(3)   VALUE SPACE.                
040600       05 FILLER                   PIC X(25)  VALUE                       
040700              'South America            '.                                
040800                                                                          
040900 01 FILLER REDEFINES RUB5.                                                
041000     03 RUB-5 OCCURS 5.                                                   
041100       05 FILLER                   PIC X(3).                              
041200       05 L1-EMBARQ-RUB5-1         PIC X(25).                             
041300                                                                          
041400 01 RUB6.                                                                 
041500     03 RUB6-ES.                                                          
041600       05 FILLER                   PIC X(3)   VALUE SPACE.                
041700       05 FILLER                   PIC X(26)  VALUE                       
041800              'GUADALAJARA '.                                             
041900                                                                          
042000     03 RUB6-AT.                                                          
042100       05 FILLER                   PIC X(3)   VALUE SPACE.                
042200       05 FILLER                   PIC X(26)  VALUE                       
042300              '                          '.                               
042400                                                                          
042500     03 RUB6-JP.                                                          
042600       05 FILLER                   PIC X(3)   VALUE SPACE.                
042700       05 FILLER                   PIC X(26)  VALUE                       
042800              '                          '.                               
042900                                                                          
043000     03 RUB6-AU.                                                          
043100       05 FILLER                   PIC X(3)   VALUE SPACE.                
043200       05 FILLER                   PIC X(26)  VALUE                       
043300              '                          '.                               
043400                                                                          
043500     03 RUB6-SE.                                                          
043600       05 FILLER                   PIC X(3)   VALUE SPACE.                
043700       05 FILLER                   PIC X(26)  VALUE                       
043800              '                          '.                               
043900                                                                          
044000 01 FILLER REDEFINES RUB6.                                                
044100     03 RUB-6 OCCURS 5.                                                   
044200       05 FILLER                   PIC X(3).                              
044300       05 L1-EMBARQ-RUB6-1         PIC X(26).                             
044400                                                                          
044500 01 RUB7.                                                                 
044600     03 RUB7-ES.                                                          
044700       05 FILLER                   PIC X(3)   VALUE SPACE.                
044800       05 FILLER                   PIC X(25)  VALUE                       
044900              'TELF.                    '.                                
045000       05 FILLER                   PIC X(3)   VALUE SPACE.                
045100       05 FILLER                   PIC X(25)  VALUE                       
045200              'FAX                      '.                                
045300                                                                          
045400     03 RUB7-AT.                                                          
045500       05 FILLER                   PIC X(3)   VALUE SPACE.                
045600       05 FILLER                   PIC X(25)  VALUE                       
045700              'Telefon:                 '.                                
045800       05 FILLER                   PIC X(3)   VALUE SPACE.                
045900       05 FILLER                   PIC X(25)  VALUE                       
046000              'Fax:                     '.                                
046100                                                                          
046200     03 RUB7-JP.                                                          
046300       05 FILLER                   PIC X(3)   VALUE SPACE.                
046400       05 FILLER                   PIC X(25)  VALUE                       
046500              'Telephone:               '.                                
046600       05 FILLER                   PIC X(3)   VALUE SPACE.                
046700       05 FILLER                   PIC X(25)  VALUE                       
046800              'Fax:                     '.                                
046900                                                                          
047000     03 RUB7-AU.                                                          
047100       05 FILLER                   PIC X(3)   VALUE SPACE.                
047200       05 FILLER                   PIC X(25)  VALUE                       
047300              'Telephone:               '.                                
047400       05 FILLER                   PIC X(3)   VALUE SPACE.                
047500       05 FILLER                   PIC X(25)  VALUE                       
047600              'Fax:                     '.                                
047700                                                                          
047800     03 RUB7-SE.                                                          
047900       05 FILLER                   PIC X(3)   VALUE SPACE.                
048000       05 FILLER                   PIC X(25)  VALUE                       
048100              'Telephone:               '.                                
048200       05 FILLER                   PIC X(3)   VALUE SPACE.                
048300       05 FILLER                   PIC X(25)  VALUE                       
048400              'Fax:                     '.                                
048500                                                                          
048600 01 FILLER REDEFINES RUB7.                                                
048700     03 RUB-7 OCCURS 5.                                                   
048800       05 FILLER                   PIC X(3).                              
048900       05 L1-EMBARQ-RUB7-1         PIC X(25).                             
049000       05 FILLER                   PIC X(3).                              
049100       05 L1-EMBARQ-RUB7-2         PIC X(25).                             
049200                                                                          
049300 01 RUB7B.                                                                
049400     03 RUB7B-ES.                                                         
049500       05 FILLER                   PIC X(3)   VALUE SPACE.                
049600       05 FILLER                   PIC X(25)  VALUE                       
049700          '949-26 60 66 '.                                                
049800       05 FILLER                   PIC X(3)   VALUE SPACE.                
049900       05 FILLER                   PIC X(25)  VALUE                       
050000          '949-26 60 31 '.                                                
050100                                                                          
050200     03 RUB7B-AT.                                                         
050300       05 FILLER                   PIC X(3)   VALUE SPACE.                
050400       05 FILLER                   PIC X(25)  VALUE                       
050500          '+43 2162 68 841'.                                              
050600       05 FILLER                   PIC X(3)   VALUE SPACE.                
050700       05 FILLER                   PIC X(25)  VALUE                       
050800          '+43 2162 68 4111'.                                             
050900                                                                          
051000     03 RUB7B-JP.                                                         
051100       05 FILLER                   PIC X(3)   VALUE SPACE.                
051200       05 FILLER                   PIC X(25)  VALUE                       
051300          '+81(5675)5-2879'.                                              
051400       05 FILLER                   PIC X(3)   VALUE SPACE.                
051500       05 FILLER                   PIC X(25)  VALUE                       
051600          '+81(5675)5-2336'.                                              
051700                                                                          
051800     03 RUB7B-AU.                                                         
051900       05 FILLER                   PIC X(3)   VALUE SPACE.                
052000       05 FILLER                   PIC X(25)  VALUE                       
052100          '+61-2-9827-3100'.                                              
052200       05 FILLER                   PIC X(3)   VALUE SPACE.                
052300       05 FILLER                   PIC X(25)  VALUE                       
052400          '+61-2-9827-3143'.                                              
052500                                                                          
052600     03 RUB7B-SE.                                                         
052700       05 FILLER                   PIC X(3)   VALUE SPACE.                
052800       05 FILLER                   PIC X(25)  VALUE                       
052900          '               '.                                              
053000       05 FILLER                   PIC X(3)   VALUE SPACE.                
053100       05 FILLER                   PIC X(25)  VALUE                       
053200          '               '.                                              
053300                                                                          
053400 01 FILLER REDEFINES RUB7B.                                               
053500     03 RUB-7B OCCURS 5.                                                  
053600       05 FILLER                   PIC X(3).                              
053700       05 L1-TELENR-RUB7B          PIC X(25).                             
053800       05 FILLER                   PIC X(3).                              
053900       05 L1-FAXNR-RUB7B           PIC X(25).                             
054000                                                                          
054100 01 RUB8.                                                                 
054200     03 RUB8-ES.                                                          
054300       05 FILLER                   PIC X(3)   VALUE SPACE.                
054400       05 FILLER                   PIC X(15)  VALUE                       
054500              'TRANSPORTISTA: '.                                          
054600       05 FILLER                   PIC X(18)  VALUE SPACE.                
054700       05 FILLER                   PIC X(2)   VALUE SPACE.                
054800       05 FILLER                   PIC X(13)  VALUE                       
054900              '     CODIGO: '.                                            
055000       05 FILLER                   PIC X(3)   VALUE SPACE.                
055100                                                                          
055200     03 RUB8-AT.                                                          
055300       05 FILLER                   PIC X(3)   VALUE SPACE.                
055400       05 FILLER                   PIC X(15)  VALUE                       
055500              'Frachter:      '.                                          
055600       05 FILLER                   PIC X(18)  VALUE SPACE.                
055700       05 FILLER                   PIC X(2)   VALUE SPACE.                
055800       05 FILLER                   PIC X(13)  VALUE                       
055900              'Fracht Code: '.                                            
056000       05 FILLER                   PIC X(3)   VALUE SPACE.                
056100                                                                          
056200     03 RUB8-JP.                                                          
056300       05 FILLER                   PIC X(3)   VALUE SPACE.                
056400       05 FILLER                   PIC X(15)  VALUE                       
056500              'Carrier:       '.                                          
056600       05 FILLER                   PIC X(18)  VALUE SPACE.                
056700       05 FILLER                   PIC X(2)   VALUE SPACE.                
056800       05 FILLER                   PIC X(13)  VALUE                       
056900              'Carrier No.  '.                                            
057000       05 FILLER                   PIC X(3)   VALUE SPACE.                
057100                                                                          
057200     03 RUB8-AU.                                                          
057300       05 FILLER                   PIC X(3)   VALUE SPACE.                
057400       05 FILLER                   PIC X(15)  VALUE                       
057500              'Carrier:       '.                                          
057600       05 FILLER                   PIC X(18)  VALUE SPACE.                
057700       05 FILLER                   PIC X(2)   VALUE SPACE.                
057800       05 FILLER                   PIC X(13)  VALUE                       
057900              'Carrier No.  '.                                            
058000       05 FILLER                   PIC X(3)   VALUE SPACE.                
058100                                                                          
058200     03 RUB8-SE.                                                          
058300       05 FILLER                   PIC X(3)   VALUE SPACE.                
058400       05 FILLER                   PIC X(15)  VALUE                       
058500              'Carrier:       '.                                          
058600       05 FILLER                   PIC X(18)  VALUE SPACE.                
058700       05 FILLER                   PIC X(2)   VALUE SPACE.                
058800       05 FILLER                   PIC X(13)  VALUE                       
058900              'Carrier No.  '.                                            
059000       05 FILLER                   PIC X(3)   VALUE SPACE.                
059100                                                                          
059200 01 FILLER REDEFINES RUB8.                                                
059300     03 RUB-8 OCCURS 5.                                                   
059400       05 FILLER                   PIC X(3).                              
059500       05 L1-EMBARQ-RUB8-1         PIC X(15).                             
059600       05 L1-TRANSPORTER-RUB8      PIC X(18).                             
059700       05 FILLER                   PIC X(2).                              
059800       05 L1-EMBARQ-RUB8-2         PIC X(13).                             
059900       05 L1-FRAKTKODER-RUB8       PIC X(3).                              
060000                                                                          
060100********* MITTSEKTIONEN PÅ LISTAN *******************                     
060200                                                                          
060300 01 RUB9.                                                                 
060400     03 RUB9-ES.                                                          
060500       05 FILLER                   PIC X(3)   VALUE SPACE.                
060600       05 FILLER                   PIC X(09)  VALUE                       
060700              '   LINE  '.                                                
060800       05 FILLER                   PIC X(07)  VALUE                       
060900              'CLIENTE'.                                                  
061000       05 FILLER                   PIC X(03)  VALUE SPACE.                
061100       05 FILLER                   PIC X(07)  VALUE                       
061200              'PEDIDO '.                                                  
061300       05 FILLER                   PIC X(11)  VALUE                       
061400              ' NO CAJON  '.                                              
061500       05 FILLER                   PIC X(20)  VALUE                       
061600              'NOMBRE DE CLIENTE   '.                                     
061700       05 FILLER                   PIC X(34)  VALUE SPACE.                
061800       05 FILLER                   PIC X(13)  VALUE                       
061900              '       PESO  '.                                            
062000       05 FILLER                   PIC X(09)  VALUE                       
062100              '  VOLUMEN'.                                                
062200       05 FILLER                   PIC X(07)  VALUE                       
062300              '  cajon'.                                                  
062400                                                                          
062500     03 RUB9-AT.                                                          
062600       05 FILLER                   PIC X(3)   VALUE SPACE.                
062700       05 FILLER                   PIC X(09)  VALUE                       
062800              '  Linie  '.                                                
062900       05 FILLER                   PIC X(07)  VALUE                       
063000              'Händler'.                                                  
063100       05 FILLER                   PIC X(03)  VALUE SPACE.                
063200       05 FILLER                   PIC X(07)  VALUE                       
063300              ' Order '.                                                  
063400       05 FILLER                   PIC X(11)  VALUE                       
063500              ' Kolli Nr  '.                                              
063600       05 FILLER                   PIC X(20)  VALUE                       
063700              'Händler Name        '.                                     
063800       05 FILLER                   PIC X(34)  VALUE SPACE.                
063900       05 FILLER                   PIC X(13)  VALUE                       
064000              '    Gewicht  '.                                            
064100       05 FILLER                   PIC X(09)  VALUE                       
064200              '  Volumen'.                                                
064300       05 FILLER                   PIC X(07)  VALUE                       
064400              '  Kolli'.                                                  
064500                                                                          
064600     03 RUB9-JP.                                                          
064700       05 FILLER                   PIC X(3)   VALUE SPACE.                
064800       05 FILLER                   PIC X(09)  VALUE                       
064900              '  Line   '.                                                
065000       05 FILLER                   PIC X(07)  VALUE                       
065100              'Dealer '.                                                  
065200       05 FILLER                   PIC X(03)  VALUE SPACE.                
065300       05 FILLER                   PIC X(07)  VALUE                       
065400              ' Order '.                                                  
065500       05 FILLER                   PIC X(11)  VALUE                       
065600              ' Case No.  '.                                              
065700       05 FILLER                   PIC X(20)  VALUE                       
065800              'Dealer Name         '.                                     
065900       05 FILLER                   PIC X(34)  VALUE SPACE.                
066000       05 FILLER                   PIC X(13)  VALUE                       
066100              '    Weight   '.                                            
066200       05 FILLER                   PIC X(09)  VALUE                       
066300              '  Volume '.                                                
066400       05 FILLER                   PIC X(07)  VALUE                       
066500              '  Cases'.                                                  
066600                                                                          
066700     03 RUB9-AU.                                                          
066800       05 FILLER                   PIC X(3)   VALUE SPACE.                
066900       05 FILLER                   PIC X(09)  VALUE                       
067000              '  Line   '.                                                
067100       05 FILLER                   PIC X(07)  VALUE                       
067200              'Dealer '.                                                  
067300       05 FILLER                   PIC X(03)  VALUE SPACE.                
067400       05 FILLER                   PIC X(07)  VALUE                       
067500              ' Order '.                                                  
067600       05 FILLER                   PIC X(11)  VALUE                       
067700              ' Case No.  '.                                              
067800       05 FILLER                   PIC X(20)  VALUE                       
067900              'Dealer Name         '.                                     
068000       05 FILLER                   PIC X(34)  VALUE SPACE.                
068100       05 FILLER                   PIC X(13)  VALUE                       
068200              '    Weight   '.                                            
068300       05 FILLER                   PIC X(09)  VALUE                       
068400              '  Volume '.                                                
068500       05 FILLER                   PIC X(07)  VALUE                       
068600              '  Cases'.                                                  
068700                                                                          
068800     03 RUB9-SE.                                                          
068900       05 FILLER                   PIC X(3)   VALUE SPACE.                
069000       05 FILLER                   PIC X(09)  VALUE                       
069100              '  Line   '.                                                
069200       05 FILLER                   PIC X(07)  VALUE                       
069300              'Dealer '.                                                  
069400       05 FILLER                   PIC X(03)  VALUE SPACE.                
069500       05 FILLER                   PIC X(07)  VALUE                       
069600              ' Order '.                                                  
069700       05 FILLER                   PIC X(11)  VALUE                       
069800              ' Case No.  '.                                              
069900       05 FILLER                   PIC X(20)  VALUE                       
070000              'Dealer Name         '.                                     
070100       05 FILLER                   PIC X(34)  VALUE SPACE.                
070200       05 FILLER                   PIC X(13)  VALUE                       
070300              '    Weight   '.                                            
070400       05 FILLER                   PIC X(09)  VALUE                       
070500              '  Volume '.                                                
070600       05 FILLER                   PIC X(07)  VALUE                       
070700              '  Cases'.                                                  
070800                                                                          
070900 01 FILLER REDEFINES RUB9.                                                
071000     03 RUB-9 OCCURS 5.                                                   
071100       05 FILLER                   PIC X(3).                              
071200       05 L1-EMBARQ-RUB9-1         PIC X(09).                             
071300       05 L1-EMBARQ-RUB9-2         PIC X(07).                             
071400       05 FILLER                   PIC X(03).                             
071500       05 L1-EMBARQ-RUB9-3         PIC X(07).                             
071600       05 L1-EMBARQ-RUB9-4         PIC X(11).                             
071700       05 L1-EMBARQ-RUB9-5         PIC X(20).                             
071800       05 FILLER                   PIC X(34).                             
071900       05 L1-EMBARQ-RUB9-6         PIC X(13).                             
072000       05 L1-EMBARQ-RUB9-7         PIC X(09).                             
072100       05 L1-EMBARQ-RUB9-8         PIC X(07).                             
072200                                                                          
072300 01 RADER.                                                                
072400     03 L1-DE-EMBARQUE-RADER.                                             
072500       05 FILLER                   PIC X(3)   VALUE SPACE.                
072600       05 L1-EMBARQ-SIDNR          PIC Z(6)9.                             
072700       05 FILLER                   PIC X(2)   VALUE SPACE.                
072800       05 L1-EMBARQ-IDDEALER       PIC Z(6)9.                             
072900       05 FILLER                   PIC X(2)   VALUE SPACE.                
073000       05 L1-EMBARQ-IDORDNR7       PIC Z(6)9.                             
073100       05 FILLER                   PIC X(5)   VALUE SPACE.                
073200       05 L1-EMBARQ-IDKOLLI        PIC Z(4)9.                             
073300       05 FILLER                   PIC X(2)   VALUE SPACE.                
073400       05 L1-EMBARQ-BEGODSM-1      PIC X(27)  VALUE SPACE.                
073500       05 FILLER                   PIC X(1)   VALUE SPACE.                
073600       05 L1-EMBARQ-BEGODSM-2      PIC X(27)  VALUE SPACE.                
073700       05 FILLER                   PIC X(02)  VALUE SPACE.                
073800       05 L1-EMBARQ-VKORDBTO       PIC Z(5)9.9.                           
073900       05 FILLER                   PIC X(3)   VALUE SPACE.                
074000       05 L1-EMBARQ-VLORDBTO       PIC Z(3)9.9(3).                        
074100                                                                          
074200                                                                          
074300                                                                          
074400 01 RADER2.                                                               
074500     03 L1-DE-EMBARQUE-RADER-2.                                           
074600       05 FILLER                   PIC X(3)   VALUE SPACE.                
074700       05 FILLER                   PIC X(37)   VALUE SPACE.               
074800       05 L1-EMBARQ-ADGODSMK-1     PIC X(27)  VALUE SPACE.                
074900       05 FILLER                   PIC X(19)   VALUE SPACE.               
075000                                                                          
075100******************************************************************        
075200                                                                          
075300********* AVSLUTNINGSDELEN PÅ LISTAN *******************                  
075400                                                                          
075500 01 RADER3.                                                               
075600     03 L1-EMBARQ-TOTAL-RAD.                                              
075700       05 FILLER                   PIC X(3)   VALUE SPACE.                
075800       05 FILLER                   PIC X(72)  VALUE SPACE.                
075900       05 TOT-PREL-RUB             PIC X(12)  VALUE                       
076000               '     TOTAL: '.                                            
076100       05 FILLER                   PIC X(10)  VALUE SPACE.                
076200       05 TOT-VKORDBTO-VIKT        PIC Z(5)9.9.                           
076300       05 FILLER                   PIC X(3)   VALUE SPACE.                
076400       05 TOT-VLORDBTO-VOLYM       PIC Z(3)9.9(3).                        
076500       05 FILLER                   PIC X(3)   VALUE SPACE.                
076600       05 TOT-KVKOLLI              PIC Z(4)9.                             
076700                                                                          
076800 01 RAD1.                                                                 
076900     03 RAD1-ES.                                                          
077000       05 FILLER                   PIC X(3)   VALUE SPACE.                
077100       05 FILLER                   PIC X(23)  VALUE                       
077200               '   NOMBRE CHOFER       '.                                 
077300       05 FILLER                   PIC X(23)  VALUE                       
077400               '               FIRMA   '.                                 
077500                                                                          
077600     03 RAD1-AT.                                                          
077700       05 FILLER                   PIC X(3)   VALUE SPACE.                
077800       05 FILLER                   PIC X(23)  VALUE                       
077900               '   übernahme Transp.:  '.                                 
078000       05 FILLER                   PIC X(23)  VALUE                       
078100               '                       '.                                 
078200                                                                          
078300     03 RAD1-JP.                                                          
078400       05 FILLER                   PIC X(3)   VALUE SPACE.                
078500       05 FILLER                   PIC X(23)  VALUE                       
078600               '   Driver Name:        '.                                 
078700       05 FILLER                   PIC X(23)  VALUE                       
078800               '                       '.                                 
078900                                                                          
079000     03 RAD1-AU.                                                          
079100       05 FILLER                   PIC X(3)   VALUE SPACE.                
079200       05 FILLER                   PIC X(23)  VALUE                       
079300               '   Driver Name:        '.                                 
079400       05 FILLER                   PIC X(23)  VALUE                       
079500               '                       '.                                 
079600                                                                          
079700     03 RAD1-SE.                                                          
079800       05 FILLER                   PIC X(3)   VALUE SPACE.                
079900       05 FILLER                   PIC X(23)  VALUE                       
080000               '   Driver Name:        '.                                 
080100       05 FILLER                   PIC X(23)  VALUE                       
080200               '                       '.                                 
080300                                                                          
080400 01 FILLER REDEFINES RAD1.                                                
080500     03 RAD-1 OCCURS 5.                                                   
080600       05 FILLER                   PIC X(3).                              
080700       05 TOT-EMBARQ-FIRMA-RUB1    PIC X(23).                             
080800       05 TOT-EMBARQ-FIRMA-RUB2    PIC X(23).                             
080900                                                                          
081000 01 RAD2.                                                                 
081100     03 RAD2-ES.                                                          
081200       05 FILLER                   PIC X(3)   VALUE SPACE.                
081300       05 FILLER                   PIC X(39)  VALUE SPACE.                
081400       05 FILLER                   PIC X(26)  VALUE                       
081500               '* * * FIN DEL INFOME * * *'.                              
081600                                                                          
081700     03 RAD2-AT.                                                          
081800       05 FILLER                   PIC X(3)   VALUE SPACE.                
081900       05 FILLER                   PIC X(39)  VALUE SPACE.                
082000       05 FILLER                   PIC X(26)  VALUE                       
082100               '* * * END OF INFO  * * * *'.                              
082200                                                                          
082300     03 RAD2-JP.                                                          
082400       05 FILLER                   PIC X(3)   VALUE SPACE.                
082500       05 FILLER                   PIC X(39)  VALUE SPACE.                
082600       05 FILLER                   PIC X(26)  VALUE                       
082700               '* * * END OF INFO  * * * *'.                              
082800                                                                          
082900     03 RAD2-AU.                                                          
083000       05 FILLER                   PIC X(3)   VALUE SPACE.                
083100       05 FILLER                   PIC X(39)  VALUE SPACE.                
083200       05 FILLER                   PIC X(26)  VALUE                       
083300               '* * * END OF INFO  * * * *'.                              
083400                                                                          
083500     03 RAD2-SE.                                                          
083600       05 FILLER                   PIC X(3)   VALUE SPACE.                
083700       05 FILLER                   PIC X(39)  VALUE SPACE.                
083800       05 FILLER                   PIC X(26)  VALUE                       
083900               '* * * END OF INFO  * * * *'.                              
084000                                                                          
084100 01 FILLER REDEFINES RAD2.                                                
084200     03 RAD-2 OCCURS 5.                                                   
084300       05 FILLER                   PIC X(3).                              
084400       05 FILLER                   PIC X(39).                             
084500       05 TOT-EMBARQ-FINI-RUB1     PIC X(26).                             
084600                                                                          
084700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
084800*                                                                         
084900     EJECT                                                                
085000 01  FILLER                      PIC X(16)   VALUE 'IMS-NYCKLAR'.         
085100     SKIP3                                                                
085200 01  NYCKLAR-TILL-DLI.                                                    
085300     03  W-WDGXKEY-X.                                                     
085400         05  FILLER              PIC X(4)    VALUE '4495'.                
085500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
085600         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO COMP-3.           
085700         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
085800         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
085900     03  W-IDGMT-X.                                                       
086000         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
086100         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
086200                                                                          
086300     03  W-IDPRODNR-X.                                                    
086400         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
086500                                                                          
086600 01  FILLER                      PIC X(16) VALUE 'IMS-STATUS-KOD'.        
086700     SKIP2                                                                
086800*    --- STATUS-KOD FRÅN IMS                                              
086900 01  STATUS-WS                   PIC XX.                                  
087000     88  SEGMENT-FINNS                       VALUE '  '.                  
087100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
087200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
087300     SKIP2                                                                
087400 01  GODK-STATUSKODER.                                                    
087500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
087600     SKIP3                                                                
087700 01  SSA1                        PIC X(64).                               
087800 01  SSA2                        PIC X(64).                               
087900     EJECT                                                                
088000*    --- IMS FUNKTIONSKODER                                               
088100*01  -COPY W0003                                                          
088200     EJECT                                                                
088300*    ---  DLI INPUT-OUTPUT AREA                                           
088400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
088500     SKIP3                                                                
088600 01  DLI-IO-AREA.                                                         
088700     03  WL449501.                                                        
088800*        05  -COPY WDGX4495 -PRE 4495-                                    
088900 01  DLI-IO-AREA1.                                                        
089000     03  WL449511.                                                        
089100*        05  -COPY WDGX4496 -PRE 4495-                                    
089200     EJECT                                                                
089300     SKIP3                                                                
089400 01  DLI-IO-AREA2.                                                        
089500     03  WL449512.                                                        
089600*        05  -COPY WDGX4498 -PRE 4495-                                    
089700 01  DLI-IO-AREA3.                                                        
089800     03  WLGMTA01.                                                        
089900*        05  -COPY WDB201                                                 
090000     EJECT                                                                
090100 LINKAGE SECTION.                                                         
090200                                                                          
090300*01  -COPY W0009   -PRE MSG-                                              
090400*01  -COPY W0009   -PRE ALT-                                              
090500     EJECT                                                                
090600*01  -COPY W0008   -PRE USEA-                                             
090700     05  FILLER                  PIC X.                                   
090800                                                                          
090900*01  -COPY W0008  -PRE 4495-                                              
091000     05  FILLER                  PIC X.                                   
091100     EJECT                                                                
091200*01  -COPY W0008  -PRE GMTA-                                              
091300     05  FILLER                  PIC X.                                   
091400                                                                          
091500     EJECT                                                                
091600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB 4495-PCB              
091700     GMTA-PCB.                                                            
091800 MAIN SECTION.                                                            
091900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB 4495-PCB              
092000     GMTA-PCB.                                                            
092100                                                                          
092200                                                                          
092300     PERFORM IMS-GET-MSG                                                  
092400     IF SEGMENT-FINNS                                                     
092500       PERFORM A-INIT                                                     
092600       PERFORM B-KOLLA-NYCKLAR                                            
092700       IF NYCKLAR-OK                                                      
092800         IF SDC-ES OR SDC-AT OR NDC-JP OR NDC-AU OR CDC-SE                
092900            PERFORM F-LAES-VISA-INFO                                      
093000         ELSE                                                             
093100            MOVE NO-PRINTING   TO MED-IDMFSINF                            
093200            CALL WMEDKONV USING MED-WMEDAREA                              
093300            MOVE MED-MFSINF TO MOD-TEMFSINF                               
093400         END-IF                                                           
093500       ELSE                                                               
093600          MOVE ERR-WRONG-KEY  TO MED-IDMFSINF                             
093700          CALL WMEDKONV USING MED-WMEDAREA                                
093800          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
093900       END-IF                                                             
094000       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O66402 + 4                      
094100       PERFORM IMS-INSERT-MSG                                             
094200     END-IF                                                               
094300                                                                          
094400     MOVE ZERO TO RETURN-CODE                                             
094500     GOBACK                                                               
094600     .                                                                    
094700     EJECT                                                                
094800 A-INIT SECTION.                                                          
094900                                                                          
095000     IF MSG-DUBBLA-TRANSKODER                                             
095100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I66401                 
095200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
095300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
095400     ELSE                                                                 
095500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I66401                  
095600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
095700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
095800     END-IF                                                               
095900                                                                          
096000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
096100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
096200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
096300                                                                          
096400     MOVE LOW-VALUE TO MSG-AREA                                           
096500     MOVE 'W4O66402' TO MFS-IDMOD                                         
096600     MOVE '4664' TO MOD-IDTRANS                                           
096700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
096800                                                                          
096900     IF EGEN-MID OR HELP-MID                                              
097000       CONTINUE                                                           
097100     ELSE                                                                 
097200       MOVE SPACE TO MFS-KDTRTYP                                          
097300       MOVE '7' TO MFS-IDPFK                                              
097400     END-IF                                                               
097500                                                                          
097600     IF ENGLISH-TEXT                                                      
097700        MOVE +2  TO SPRAK-IX                                              
097800        MOVE 'GB ' TO MED-IDSKYLT                                         
097900     ELSE                                                                 
098000        MOVE +1  TO SPRAK-IX                                              
098100        MOVE 'S  ' TO MED-IDSKYLT                                         
098200     END-IF                                                               
098300                                                                          
098400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
098500     MOVE '001'             TO MSGI-KDCALL                                
098600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
098700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
098800     MOVE MSGI-TILOKDAT     TO DAGENS-DATUM                               
098900     MOVE MSGI-TILOKTID     TO DAGENS-TID-LOKAL                           
099000                                                                          
099100*==> SKAPA SEKEL                                                          
099200     MOVE 'IDAG  '  TO DAT-KDDATFORM                                      
099300     CALL WDATKONV USING DAT-KDDATFORM                                    
099400                           DAT-I-TIDATUM                                  
099500                           DAT-O-TIDATUM                                  
099600                           DAT-KDSVAR                                     
099700     IF DAT-KDSVAR-OK                                                     
099800        MOVE DAT-TISEKEL  TO WS-SEKEL                                     
099900     ELSE                                                                 
100000        MOVE ZERO         TO WS-SEKEL                                     
100100     END-IF                                                               
100200                                                                          
100300     MOVE DAGENS-AA  TO WS-AAR                                            
100400     MOVE DAGENS-MM  TO WS-MAN                                            
100500     MOVE DAGENS-DD  TO WS-DAG                                            
100600     MOVE ZERO       TO WS-SID-RAKNARE                                    
100700     MOVE ZERO       TO WS-RAD-RAKNARE                                    
100800     MOVE +0         TO RAD-INDX                                          
100900     .                                                                    
101000     EJECT                                                                
101100 B-KOLLA-NYCKLAR SECTION.                                                 
101200                                                                          
101300     MOVE JA TO NYCKLAR-SW                                                
101400                                                                          
101500     IF MID-IDTRPTNR-IN     =  ALL '+'                                    
101600       MOVE MID-IDTRPTNR-UT TO WS-IDTRPTNR                                
101700       INSPECT WS-IDTRPTNR REPLACING LEADING SPACE BY ZERO                
101800     ELSE                                                                 
101900       MOVE MID-IDTRPTNR-IN TO WS-IDTRPTNR                                
102000       MOVE '7'             TO MFS-IDPFK                                  
102100       MOVE SPACE           TO MFS-KDTRTYP                                
102200     END-IF                                                               
102300                                                                          
102400     IF WS-IDTRPTNR NUMERIC AND WS-IDTRPTNR > ZERO                        
102500       MOVE WS-IDTRPTNR     TO W-IDTRPTNR                                 
102600     ELSE                                                                 
102700       MOVE NEJ             TO NYCKLAR-SW                                 
102800     END-IF                                                               
102900                                                                          
103000     IF MID-IDLBBET-IN       = ALL '+'                                    
103100       MOVE MID-IDLBBET-UT   TO WS-IDLBBET                                
103200     ELSE                                                                 
103300       MOVE MID-IDLBBET-IN   TO WS-IDLBBET                                
103400       MOVE '7'              TO MFS-IDPFK                                 
103500       MOVE SPACE            TO MFS-KDTRTYP                               
103600     END-IF                                                               
103700     IF WS-IDLBBET           NOT = SPACE                                  
103800       MOVE WS-IDLBBET       TO W-IDLBBET                                 
103900     ELSE                                                                 
104000       MOVE NEJ              TO NYCKLAR-SW                                
104100     END-IF                                                               
104200                                                                          
104300     IF MID-FLFARLIG-IN      = ALL '+'                                    
104400       MOVE MID-FLFARLIG-UT  TO WS-FLFARLIG                               
104500     ELSE                                                                 
104600       MOVE MID-FLFARLIG-IN  TO WS-FLFARLIG                               
104700       MOVE '7'              TO MFS-IDPFK                                 
104800       MOVE SPACE            TO MFS-KDTRTYP                               
104900     END-IF                                                               
105000     IF WS-FLFARLIG          = SPACE                                      
105100       MOVE NEJ              TO NYCKLAR-SW                                
105200     END-IF                                                               
105300                                                                          
105400                                                                          
105500     MOVE MSGI-IDDC         TO W-IDDC                                     
105600                               WS-IDDC                                    
105700                                                                          
105800     IF CDC-SE                                                            
105900*       TEST OM SYD-AMERIKA                                               
106000     PERFORM IMS-GET-4495-WL449501                                        
106100     IF SEGMENT-FINNS                                                     
106200       PERFORM IMS-GET-4495-WL449511                                      
106300       IF SEGMENT-FINNS                                                   
106400         PERFORM IMS-GNP-4495-WL449512                                    
106500         IF SEGMENT-FINNS                                                 
106600           IF 4495-4498-IDDISTR = 6311 OR 6480 OR 6580 OR                 
106700                                  6587 OR 6560 OR 6561 OR                 
106800                                  6680 OR 6785 OR 6898 OR                 
106900                                  7040 OR 7412 OR 7416 OR                 
107000                                  7436 OR 7470 OR 7480 OR                 
107100                                  7482 OR 7490 OR 7496 OR                 
107200                                  7538 OR 6588 OR 6589 OR                 
107210                                  6590 OR 6591 OR                         
107300                                  7050 OR 7051                            
107400             CONTINUE                                                     
107500           ELSE                                                           
107600             MOVE NEJ           TO NYCKLAR-SW                             
107700           END-IF                                                         
107800         END-IF                                                           
107900       END-IF                                                             
108000     END-IF                                                               
108100     END-IF                                                               
108200     .                                                                    
108300     EJECT                                                                
108400 F-LAES-VISA-INFO SECTION.                                                
108500                                                                          
108600     PERFORM IMS-GET-4495-WL449501                                        
108700                                                                          
108800     IF SEGMENT-FINNS                                                     
108900                                                                          
109000        IF SDC-ES                                                         
109100          MOVE 1  TO TEXT-IX                                              
109200        ELSE                                                              
109300          IF SDC-AT                                                       
109400            MOVE 2 TO TEXT-IX                                             
109500          ELSE                                                            
109600            IF NDC-JP                                                     
109700              MOVE 3 TO TEXT-IX                                           
109800            ELSE                                                          
109900              IF NDC-AU                                                   
110000                MOVE 4 TO TEXT-IX                                         
110100              ELSE                                                        
110200                IF CDC-SE                                                 
110300                  MOVE 5 TO TEXT-IX                                       
110400                END-IF                                                    
110500              END-IF                                                      
110600            END-IF                                                        
110700          END-IF                                                          
110800        END-IF                                                            
110900                                                                          
111000        PERFORM FA-OPEN-PRINTING                                          
111100                                                                          
111200        PERFORM S01-SKRIV-LIST-HUVUD                                      
111300                                                                          
111400        PERFORM IMS-GET-4495-WL449511                                     
111500        IF SEGMENT-FINNS                                                  
111600           MOVE 4495-4496-VKORDBTO-LASTB                                  
111700                                     TO WS-VKORDBTO-TOT                   
111800           MOVE 4495-4496-VLORDBTO-LASTB                                  
111900                                     TO WS-VLORDBTO-TOT                   
112000           MOVE 4495-4496-KVKOLLI-LAST                                    
112100                                     TO WS-KVKOLLI-TOT                    
112200           PERFORM IMS-GNP-4495-WL449512                                  
112300           IF SEGMENT-FINNS                                               
112400              MOVE 4495-4498-IDDISTR TO W-IDDISTR-WDB2                    
112500              MOVE 4495-4498-IDPRODNR TO W-IDPRODNR                       
112600              MOVE 4495-4498-IDDEALER TO W-IDKUNDNR-WDB2                  
112700                                   L1-EMBARQ-IDDEALER                     
112800                                       SPAR-IDDEALER                      
112900              MOVE 'J'                TO WS-ADRESS                        
113000              PERFORM FC-HAMTA-4498INFO                                   
113100              PERFORM S01B-SKRIV-LIST-HUVUD                               
113200              PERFORM FD-HAMTA-KUNDINFO                                   
113300              PERFORM S03A-SKRIV-OVERSKRIFT-LIRAD                         
113400                                                                          
113500              PERFORM FH-HAEMTA-LISTRADER                                 
113600              MOVE START-PRINT                                            
113700                           TO MED-IDMFSINF                                
113800              CALL WMEDKONV USING MED-WMEDAREA                            
113900              MOVE MED-MFSINF TO MOD-TEMFSINF                             
114000           ELSE                                                           
114100              MOVE ERR-WRONG-KEY                                          
114200                           TO MED-IDMFSINF                                
114300              CALL WMEDKONV USING MED-WMEDAREA                            
114400              MOVE MED-MFSINF TO MOD-TEMFSINF                             
114500           END-IF                                                         
114600                                                                          
114700           PERFORM FG-KOLLA-SISTA-SIDA                                    
114800        ELSE                                                              
114900           MOVE ERR-WRONG-KEY                                             
115000                           TO MED-IDMFSINF                                
115100           CALL WMEDKONV USING MED-WMEDAREA                               
115200           MOVE MED-MFSINF TO MOD-TEMFSINF                                
115300        END-IF                                                            
115400        PERFORM FB-CLOSE-PRINTING                                         
115500     ELSE                                                                 
115600        MOVE ERR-WRONG-KEY                                                
115700                           TO MED-IDMFSINF                                
115800        CALL WMEDKONV USING MED-WMEDAREA                                  
115900        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
116000     END-IF                                                               
116100                                                                          
116200                                                                          
116300     .                                                                    
116400     EJECT                                                                
116500 FA-OPEN-PRINTING SECTION.                                                
116600                                                                          
116700*---- ÖPPNAR SUBPROGRAM FÖR PRINTNING --                                  
116800                                                                          
116900     IF SDC-ES                                                            
117000       MOVE 'ES8'     TO WS-IDPRTLST                                      
117100     ELSE                                                                 
117200       IF SDC-AT                                                          
117300         MOVE 'WI1'   TO WS-IDPRTLST                                      
117400       ELSE                                                               
117500         IF NDC-JP                                                        
117600           MOVE 'W4069961'   TO WS-IDPRTLST                               
117700         ELSE                                                             
117800           IF NDC-AU                                                      
117900             MOVE 'W4069962' TO WS-IDPRTLST                               
118000           ELSE                                                           
118100             IF CDC-SE                                                    
118200*              MOVE 'QSE10088' TO WS-IDPRTLST                             
118300               MOVE '066     ' TO WS-IDPRTLST                             
118400             END-IF                                                       
118500           END-IF                                                         
118600         END-IF                                                           
118700       END-IF                                                             
118800     END-IF                                                               
118900                                                                          
119000     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
119100                         PRT-OPEN                                         
119200                         WS-IDPRTLST                                      
119300                         ALT-PCB                                          
119400                         WS-DUMMY                                         
119500                         WS-DUMMY                                         
119600                                                                          
119700     .                                                                    
119800     EJECT                                                                
119900 FB-CLOSE-PRINTING SECTION.                                               
120000                                                                          
120100     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
120200                          PRT-CLOSE                                       
120300                          WS-IDPRTLST                                     
120400                          ALT-PCB                                         
120500                          WS-DUMMY                                        
120600                          WS-DUMMY                                        
120700     .                                                                    
120800     EJECT                                                                
120900 FC-HAMTA-4498INFO SECTION.                                               
121000                                                                          
121100     MOVE 4495-4498-IDORDNR7 TO L1-EMBARQ-IDORDNR7                        
121200     MOVE 4495-4498-IDKOLLI  TO L1-EMBARQ-IDKOLLI                         
121300     MOVE 4495-4498-VKORDBTO TO L1-EMBARQ-VKORDBTO                        
121400     MOVE 4495-4498-VLORDBTO TO L1-EMBARQ-VLORDBTO                        
121500     .                                                                    
121600     EJECT                                                                
121700 FD-HAMTA-KUNDINFO SECTION.                                               
121800                                                                          
121900     PERFORM IMS-GET-GMTA-WLGMTA01                                        
122000     IF SEGMENT-FINNS                                                     
122100        MOVE GMT-BEGMT-RAD1  TO                                           
122200                             L1-EMBARQ-BEGODSM-1                          
122300        MOVE GMT-ADGMT-GATA  TO                                           
122400                             L1-EMBARQ-BEGODSM-2                          
122500        MOVE GMT-ADGMT-PADR  TO                                           
122600                             L1-EMBARQ-ADGODSMK-1                         
122700     ELSE                                                                 
122800        MOVE SPACE           TO                                           
122900                             L1-EMBARQ-BEGODSM-1                          
123000                             L1-EMBARQ-BEGODSM-2                          
123100                             L1-EMBARQ-ADGODSMK-1                         
123200     END-IF                                                               
123300                                                                          
123400     .                                                                    
123500     EJECT                                                                
123600 FF-KOLLA-SIDBYTE SECTION.                                                
123700                                                                          
123800     IF WS-RAD-RAKNARE > 42                                               
123900        PERFORM S01-SKRIV-LIST-HUVUD                                      
124000        PERFORM S01B-SKRIV-LIST-HUVUD                                     
124100        PERFORM S03A-SKRIV-OVERSKRIFT-LIRAD                               
124200        ADD +1                TO RAD-INDX                                 
124300        PERFORM S03B-SKRIV-LISTRADER                                      
124400     ELSE                                                                 
124500        ADD +1                TO RAD-INDX                                 
124600        PERFORM S03B-SKRIV-LISTRADER                                      
124700     END-IF                                                               
124800                                                                          
124900     .                                                                    
125000     EJECT                                                                
125100 FG-KOLLA-SISTA-SIDA SECTION.                                             
125200                                                                          
125300     IF WS-RAD-RAKNARE > 35                                               
125400        PERFORM S01-SKRIV-LIST-HUVUD                                      
125500        PERFORM S01B-SKRIV-LIST-HUVUD                                     
125600        PERFORM S03A-SKRIV-OVERSKRIFT-LIRAD                               
125700        PERFORM S04-SKRIV-LIST-AVSLUT                                     
125800     ELSE                                                                 
125900        PERFORM S04-SKRIV-LIST-AVSLUT                                     
126000     END-IF                                                               
126100     .                                                                    
126200     EJECT                                                                
126300 FH-HAEMTA-LISTRADER SECTION.                                             
126400                                                                          
126500     PERFORM UNTIL SEGMENT-SAKNAS                                         
126600                                                                          
126700        PERFORM FF-KOLLA-SIDBYTE                                          
126800        PERFORM IMS-GNP-4495-WL449512                                     
126900        IF SEGMENT-FINNS                                                  
127000           MOVE 4495-4498-IDDISTR TO W-IDDISTR-WDB2                       
127100           MOVE 4495-4498-IDPRODNR TO W-IDPRODNR                          
127200           MOVE 4495-4498-IDDEALER TO W-IDKUNDNR-WDB2                     
127300                                   L1-EMBARQ-IDDEALER                     
127400           MOVE 'N'                TO WS-ADRESS                           
127500           IF SPAR-IDDEALER NOT = W-IDKUNDNR-WDB2                         
127600             MOVE 'J'              TO WS-ADRESS                           
127700             MOVE W-IDKUNDNR-WDB2  TO SPAR-IDDEALER                       
127800           END-IF                                                         
127900           PERFORM FC-HAMTA-4498INFO                                      
128000           PERFORM FD-HAMTA-KUNDINFO                                      
128100        END-IF                                                            
128200     END-PERFORM                                                          
128300     .                                                                    
128400     EJECT                                                                
128500 S01-SKRIV-LIST-HUVUD    SECTION.                                         
128600                                                                          
128700     MOVE +0 TO WS-RAD-RAKNARE                                            
128800     ADD  +1 TO WS-SID-RAKNARE                                            
128900                                                                          
129000***** LISTA DE EMBARQUE         PAGINA:  1  ********                      
129100                                                                          
129200     MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                  
129300                                                                          
129400     MOVE WS-SID-RAKNARE  TO L1-EMBARQUE-RUB1-SIDNR (TEXT-IX)             
129500     MOVE RUB-1 (TEXT-IX)      TO LIST-RAD                                
129600                                                                          
129700     PERFORM S02-PRINTA-RAD                                               
129800     ADD  +1 TO WS-RAD-RAKNARE                                            
129900                                                                          
130000***** VOLVO ESPANA, S.A                     ********                      
130100                                                                          
130200     MOVE RUB-2 (TEXT-IX)      TO LIST-RAD                                
130300     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
130400                                                                          
130500     PERFORM S02-PRINTA-RAD                                               
130600     ADD  +1 TO WS-RAD-RAKNARE                                            
130700                                                                          
130800***** CENTRO DISTRIBUCION REPUESTO   FECHA/HORA ****                      
130900                                                                          
131000     MOVE WS-SEKEL        TO L1-RUB3-SEKEL (TEXT-IX)                      
131100     MOVE WS-AAR          TO LI-RUB3-YEAR  (TEXT-IX)                      
131200     MOVE WS-MAN          TO L1-RUB3-MANAD (TEXT-IX)                      
131300     MOVE WS-DAG          TO L1-RUB3-DAG   (TEXT-IX)                      
131400     MOVE DAGENS-HH-LOK   TO L1-RUB3-TIMME (TEXT-IX)                      
131500     MOVE DAGENS-MIN-LOK  TO L1-RUB3-MINUT (TEXT-IX)                      
131600     MOVE RUB-3 (TEXT-IX)      TO LIST-RAD                                
131700     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
131800                                                                          
131900     PERFORM S02-PRINTA-RAD                                               
132000     ADD +1 TO WS-RAD-RAKNARE                                             
132100                                                                          
132200***** AVENIDA DE LAINDUSTRIA, 16            ********                      
132300                                                                          
132400     MOVE RUB-4 (TEXT-IX)      TO LIST-RAD                                
132500     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
132600                                                                          
132700     PERFORM S02-PRINTA-RAD                                               
132800     ADD +1 TO WS-RAD-RAKNARE                                             
132900                                                                          
133000***** 19200 AZUQUECA DE HENARES             ********                      
133100                                                                          
133200     MOVE RUB-5 (TEXT-IX)      TO LIST-RAD                                
133300     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
133400                                                                          
133500     PERFORM S02-PRINTA-RAD                                               
133600     ADD +1 TO WS-RAD-RAKNARE                                             
133700                                                                          
133800***** GUADALAJARA                           ********                      
133900                                                                          
134000     MOVE RUB-6 (TEXT-IX)      TO LIST-RAD                                
134100     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
134200                                                                          
134300     PERFORM S02-PRINTA-RAD                                               
134400     ADD +1 TO WS-RAD-RAKNARE                                             
134500                                                                          
134600***** TELF.            FAX                  ********                      
134700                                                                          
134800     MOVE RUB-7 (TEXT-IX)      TO LIST-RAD                                
134900     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
135000                                                                          
135100     PERFORM S02-PRINTA-RAD                                               
135200     ADD +1 TO WS-RAD-RAKNARE                                             
135300                                                                          
135400***** 054329777                             ********                      
135500                                                                          
135600     MOVE RUB-7B (TEXT-IX)     TO LIST-RAD                                
135700     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
135800                                                                          
135900     PERFORM S02-PRINTA-RAD                                               
136000     ADD +1 TO WS-RAD-RAKNARE                                             
136100                                                                          
136200     .                                                                    
136300     EJECT                                                                
136400                                                                          
136500 S01B-SKRIV-LIST-HUVUD   SECTION.                                         
136600                                                                          
136700***** TRANSPORTISTA: JOSE MARTINEZ CONDIGO: FRAKTKOD ****                 
136800                                                                          
136900     MOVE SPACE           TO L1-TRANSPORTER-RUB8 (TEXT-IX)                
137000     MOVE WS-IDTRPTNR                                                     
137100                          TO L1-FRAKTKODER-RUB8 (TEXT-IX)                 
137200                                                                          
137300     IF SDC-ES                                                            
137400       IF WS-IDTRPTNR = '100'                                             
137500          MOVE 'GRUPO CAT      '                                          
137600                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
137700       ELSE                                                               
137800        IF WS-IDTRPTNR = '150'                                            
137900           MOVE 'VELTRANS       '                                         
138000                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
138100         ELSE                                                             
138200          IF WS-IDTRPTNR = '200'                                          
138300             MOVE 'LUALGA         '                                       
138400                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
138500          ELSE                                                            
138600            IF WS-IDTRPTNR = '300'                                        
138700               MOVE 'CTE GRAN CANARIA'                                    
138800                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
138900            ELSE                                                          
139000             IF WS-IDTRPTNR = '400'                                       
139100                MOVE 'SUS MEDIOS     '                                    
139200                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
139300             ELSE                                                         
139400              IF WS-IDTRPTNR = '500'                                      
139500                 MOVE 'GALLIKER       '                                   
139600                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
139700              ELSE                                                        
139800               IF WS-IDTRPTNR = '600'                                     
139900                  MOVE 'TAXI           '                                  
140000                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
140100               ELSE                                                       
140200                IF WS-IDTRPTNR = '700'                                    
140300                   MOVE 'IBEXPRESS      '                                 
140400                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
140500                ELSE                                                      
140600                 IF WS-IDTRPTNR = '800'                                   
140700                    MOVE 'A.V.E.         '                                
140800                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
140900                 ELSE                                                     
141000                  IF WS-IDTRPTNR = '900'                                  
141100                     MOVE 'LUALGA         '                               
141200                            TO L1-TRANSPORTER-RUB8 (TEXT-IX)              
141300                  ELSE                                                    
141400                     MOVE SPACE                                           
141500                              TO L1-TRANSPORTER-RUB8 (TEXT-IX)            
141600                  END-IF                                                  
141700                END-IF                                                    
141800               END-IF                                                     
141900              END-IF                                                      
142000             END-IF                                                       
142100            END-IF                                                        
142200           END-IF                                                         
142300          END-IF                                                          
142400        END-IF                                                            
142500       END-IF                                                             
142600     ELSE                                                                 
142700       IF SDC-AT                                                          
142800         MOVE 'Englmayer'       TO L1-TRANSPORTER-RUB8 (TEXT-IX)          
142900       ELSE                                                               
143000         IF NDC-JP                                                        
143100           IF WS-IDTRPTNR = '100'                                         
143200             MOVE 'Seino          '                                       
143300                                TO L1-TRANSPORTER-RUB8 (TEXT-IX)          
143400           ELSE                                                           
143500             IF WS-IDTRPTNR = '200'                                       
143600                MOVE 'Meitetsu       '                                    
143700                                TO L1-TRANSPORTER-RUB8 (TEXT-IX)          
143800             ELSE                                                         
143900                MOVE SPACE                                                
144000                                TO L1-TRANSPORTER-RUB8 (TEXT-IX)          
144100             END-IF                                                       
144200           END-IF                                                         
144300         ELSE                                                             
144400           IF NDC-AU                                                      
144500             MOVE 'TNT'         TO L1-TRANSPORTER-RUB8 (TEXT-IX)          
144600           ELSE                                                           
144700            IF CDC-SE                                                     
144800             MOVE '      '      TO L1-TRANSPORTER-RUB8 (TEXT-IX)          
144900            END-IF                                                        
145000           END-IF                                                         
145100         END-IF                                                           
145200       END-IF                                                             
145300     END-IF                                                               
145400                                                                          
145500     MOVE RUB-8 (TEXT-IX) TO LIST-RAD                                     
145600     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
145700                                                                          
145800     PERFORM S02-PRINTA-RAD                                               
145900     ADD +2 TO WS-RAD-RAKNARE                                             
146000                                                                          
146100     .                                                                    
146200     EJECT                                                                
146300                                                                          
146400 S02-PRINTA-RAD          SECTION.                                         
146500                                                                          
146600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-IDPRTLST              
146700                         ALT-PCB PRT-RADSKIP                              
146800                         LIST-RAD                                         
146900     .                                                                    
147000     EJECT                                                                
147100                                                                          
147200 S03A-SKRIV-OVERSKRIFT-LIRAD SECTION.                                     
147300                                                                          
147400     MOVE RUB-9 (TEXT-IX)     TO LIST-RAD                                 
147500     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
147600     PERFORM S02-PRINTA-RAD                                               
147700     ADD +2 TO WS-RAD-RAKNARE                                             
147800     .                                                                    
147900     EJECT                                                                
148000                                                                          
148100 S03B-SKRIV-LISTRADER     SECTION.                                        
148200                                                                          
148300     MOVE RAD-INDX        TO L1-EMBARQ-SIDNR                              
148400     MOVE L1-DE-EMBARQUE-RADER TO LIST-RAD                                
148500     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
148600     PERFORM S02-PRINTA-RAD                                               
148700     ADD +1 TO WS-RAD-RAKNARE                                             
148800                                                                          
148900     IF  WS-ADRESS = 'J'                                                  
149000         IF  L1-EMBARQ-BEGODSM-2 = SPACE                                  
149100         AND L1-EMBARQ-ADGODSMK-1 = SPACE                                 
149200             CONTINUE                                                     
149300         ELSE                                                             
149400           MOVE L1-DE-EMBARQUE-RADER-2 TO LIST-RAD                        
149500           MOVE PRT-AFTER-1     TO PRT-RADSKIP                            
149600           PERFORM S02-PRINTA-RAD                                         
149700           ADD +1 TO WS-RAD-RAKNARE                                       
149800         END-IF                                                           
149900     END-IF                                                               
150000     .                                                                    
150100     EJECT                                                                
150200                                                                          
150300 S04-SKRIV-LIST-AVSLUT   SECTION.                                         
150400                                                                          
150500     MOVE SPACE TO LIST-RAD                                               
150600     MOVE WS-VKORDBTO-TOT TO TOT-VKORDBTO-VIKT                            
150700     MOVE WS-VLORDBTO-TOT TO TOT-VLORDBTO-VOLYM                           
150800     MOVE WS-KVKOLLI-TOT  TO TOT-KVKOLLI                                  
150900     MOVE L1-EMBARQ-TOTAL-RAD TO LIST-RAD                                 
151000     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
151100     PERFORM S02-PRINTA-RAD                                               
151200     ADD +2 TO WS-RAD-RAKNARE                                             
151300                                                                          
151400     MOVE RAD-1 (TEXT-IX)      TO LIST-RAD                                
151500     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
151600     PERFORM S02-PRINTA-RAD                                               
151700     ADD +1 TO WS-RAD-RAKNARE                                             
151800                                                                          
151900     MOVE RAD-2 (TEXT-IX)      TO LIST-RAD                                
152000     MOVE PRT-AFTER-4     TO PRT-RADSKIP                                  
152100     PERFORM S02-PRINTA-RAD                                               
152200     ADD +4 TO WS-RAD-RAKNARE                                             
152300                                                                          
152400     .                                                                    
152500     EJECT                                                                
152600                                                                          
152700* --- IMS SEKTIONER ---                                                   
152800     SKIP3                                                                
152900 IMS-GET-MSG SECTION.                                                     
153000                                                                          
153100     MOVE '  QC' TO GODK-STATUSKODER                                      
153200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
153300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
153400     PERFORM IMS-STATUSKONTROLL                                           
153500     .                                                                    
153600     SKIP3                                                                
153700 IMS-INSERT-MSG SECTION.                                                  
153800                                                                          
153900     IF ENGLISH-TEXT                                                      
154000       MOVE 'N' TO MFS-KDHUVOMR                                           
154100     END-IF                                                               
154200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
154300     MOVE SPACE TO GODK-STATUSKODER                                       
154400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
154500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
154600     PERFORM IMS-STATUSKONTROLL                                           
154700     .                                                                    
154800     EJECT                                                                
154900 IMS-GET-4495-WL449501 SECTION.                                           
155000                                                                          
155100     STRING 'WL449501(WDGXKEY  =' W-WDGXKEY-X ')'                         
155200          DELIMITED BY SIZE INTO SSA1                                     
155300     MOVE '  GE' TO GODK-STATUSKODER                                      
155400     CALL CBLTDLI USING GU 4495-PCB DLI-IO-AREA SSA1                      
155500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
155600     PERFORM IMS-STATUSKONTROLL                                           
155700     .                                                                    
155800     EJECT                                                                
155900 IMS-GET-4495-WL449511 SECTION.                                           
156000     MOVE   'WL449511'         TO SSA1                                    
156100     MOVE '  GE' TO GODK-STATUSKODER                                      
156200     CALL CBLTDLI USING GNP 4495-PCB DLI-IO-AREA1 SSA1                    
156300     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
156400     PERFORM IMS-STATUSKONTROLL                                           
156500     .                                                                    
156600     SKIP2                                                                
156700 IMS-GNP-4495-WL449512 SECTION.                                           
156800     MOVE   'WL449512'         TO SSA1                                    
156900     MOVE '  GE' TO GODK-STATUSKODER                                      
157000     CALL CBLTDLI USING GNP 4495-PCB DLI-IO-AREA2 SSA1                    
157100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
157200     PERFORM IMS-STATUSKONTROLL                                           
157300     .                                                                    
157400     SKIP2                                                                
157500 IMS-GET-GMTA-WLGMTA01 SECTION.                                           
157600                                                                          
157700     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
157800          DELIMITED BY SIZE INTO SSA1                                     
157900     MOVE '  GE' TO GODK-STATUSKODER                                      
158000     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA3 SSA1                     
158100     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
158200     PERFORM IMS-STATUSKONTROLL                                           
158300     .                                                                    
158400     EJECT                                                                
158500 IMS-STATUSKONTROLL SECTION.                                              
158600                                                                          
158700     SET STATUS-IX TO 1                                                   
158800     SEARCH GODK-STATUS                                                   
158900       AT END                                                             
159000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
159100         DELIMITED BY SIZE INTO FELTEXT                                   
159200         CALL FELLOG                                                      
159300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
159400         CONTINUE                                                         
159500     END-SEARCH                                                           
159600     .                                                                    
