000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2366200.                                                
000400*AUTHOR.         HENRIK ARONSSON.                                         
000500*DATE-WRITTEN.   FEB 1992.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET SKRIVER UT LISTA ÖVER                                 
001100*        LEVERANSFÖRSENADE ARTIKLAR                                       
001200*                                                                         
001300*        STYRANDE FIL ÄR W23660.                                          
001400*          DENNA INNEHÅLLER FÖRSENADE INLEVERANSER.                       
001500*          FÖR VARJE ARTIKEL/LEVERANTÖR MED FÖRSENAD INLEVERANS           
001600*          FINNS ALLA DESS FÖRSENADE INLEVERANSER (PTYP = SEN)            
001700*          SAMT EN "SLUTPOST" MED NÄSTA INLEVERANS(PTYP = NST)            
001800*                                                                         
001900*        KOMPLETTERANDE FIL ÄR W23661.                                    
002000*          DENNA INEHÅLLER ALLA LEVERANSBESKED FÖR                        
002100*          ARTIKLARNA/LEVERANTÖRERNA I FIL W23660.                        
002200*                                                                         
002300*        PROGRAMMET LÄSER      WDP3                                       
002400*                                                                         
002500*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*          --- FÖRSENADE INLEVERANSER MED DIV. INFO                       
003900     SELECT W23660                     ASSIGN TO W23662D1.                
004000     SKIP2                                                                
004100*          --- WDD924-INFO (LEVERANSBESKED)                               
004200     SELECT W23661                     ASSIGN TO W23662D2.                
004300     SKIP2                                                                
004400*          --- LISTA LEVERANSFÖRSENADE ARTIKLAR                           
004500     SELECT W23662-001                 ASSIGN TO W23662D3.                
004600     SKIP2                                                                
004700*          --- UTFIL LEVERANSFÖRSENADE ARTIKLAR                           
004800     SELECT W23663                     ASSIGN TO W23662D4.                
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100     SKIP3                                                                
005200 FILE SECTION.                                                            
005300     SKIP3                                                                
005400 FD  W23660                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700     SKIP2                                                                
005800*01  -COPY W23660      -L.                                                
005900     SKIP3                                                                
006000 FD  W23661                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300     SKIP2                                                                
006400*01  -COPY W23661      -L.                                                
006500     SKIP3                                                                
006600 FD  W23662-001                                                           
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900     SKIP2                                                                
007000 01  W23662-001-RAD              PIC X(165).                              
007100     SKIP3                                                                
007200 FD  W23663                                                               
007300     RECORDING      F                                                     
007400     BLOCK CONTAINS 0.                                                    
007500     SKIP2                                                                
007600*01  POST -COPY W23663   -L  -PRE UT-                                     
007700     EJECT                                                                
007800 WORKING-STORAGE SECTION.                                                 
007900     SKIP2                                                                
008000                                                                          
008100*    -- CHECKED BY WY2000                                                 
008200 77  IDPGM                       PIC X(8)    VALUE 'W2366200'.            
008300 77  JA                          PIC X       VALUE 'J'.                   
008400 77  NEJ                         PIC X       VALUE 'N'.                   
008500 77  OLD-IDANSK                  PIC 9(3)    VALUE 999.                   
008600 77  SPAR-IDANSK                 PIC 9(3)    VALUE ZERO.                  
008700 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
008800 77  OLD-IDLEVNR                 PIC X(5)    VALUE SPACE.                 
008900 77  SPAR-IDARTNR                PIC 9(9)    VALUE ZERO.                  
009000 77  NY-ARTIKEL                  PIC X       VALUE 'N'.                   
009100 77  NY-ANSK-LEV                 PIC X       VALUE 'N'.                   
009200 77  NAESTA-INLEV                PIC X       VALUE 'N'.                   
009300 77  PUNKT                       PIC X       VALUE '.'.                   
009400 01  IX                          PIC S9(3)   COMP-3 VALUE ZERO.           
009500                                                                          
009600 77  W23660-EOF-SW               PIC X       VALUE 'N'.                   
009700     88  END-OF-W23660                       VALUE 'J'.                   
009800                                                                          
009900 77  W23661-EOF-SW               PIC X       VALUE 'N'.                   
010000     88  END-OF-W23661                       VALUE 'J'.                   
010100     EJECT                                                                
010200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010300 01  FILLER REDEFINES DAGENS-DATUM.                                       
010400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010700     EJECT                                                                
010800 01  DYNAMISKA-SUBPROGRAM.                                                
010900*                                                                         
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     SKIP2                                                                
011500*    --- PARAMETRAR TILL ABEND                                            
011600                                                                          
011700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011900     SKIP2                                                                
012000 01  FELTEXT.                                                             
012100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012300     EJECT                                                                
012400*    --- PARAMETRAR TILL POSTSUM                                          
012500*                                                                         
012600*01  -COPY W0005   -PRE  POSTSUM-                                         
012700     EJECT                                                                
012800 01  IN60-AREA-START             PIC X(24)   VALUE                        
012900                                 'IN60-AREA-START  '.                     
013000*01  AREA -COPY W23660     -PRE IN60-                                     
013100                                                                          
013200     EJECT                                                                
013300 01  IN61-AREA-START             PIC X(24)   VALUE                        
013400                                 'IN61-AREA-START  '.                     
013500*01  AREA -COPY W23661     -PRE IN61-                                     
013600                                                                          
013700     EJECT                                                                
013800                                                                          
013900 01  UT-AREA-START               PIC X(24)   VALUE                        
014000                                 'UT-AREA-START  '.                       
014100     SKIP2                                                                
014200 01  UT-AREA.                                                             
014300     03  FILLER                  PIC X(119).                              
014400*01  FILLER -COPY W23663     -PRE UT-     -RED  UT-AREA                   
014500     EJECT                                                                
014600 01  W001-AREA-START             PIC X(24)   VALUE                        
014700                                 'W001-AREA-START  '.                     
014800 01  W001-HJALPAREOR.                                                     
014900*                                                                         
015000     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
015100     03  W001-ANTAL-RADER                                                 
015200                                 PIC 9(3)    VALUE 999.                   
015300     03  W001-MAX-RADER-PER-SIDA                                          
015400                                 PIC 9(3)    VALUE 63.                    
015500     03  W001-MAX-POSITIONER-PER-RAD                                      
015600                                 PIC 9(3)    VALUE 165.                   
015700     03  W001-LISTNR             PIC X(11)   VALUE 'W23662-001'.          
015800     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
015900     EJECT                                                                
016000 01  W001-RAD.                                                            
016100*                                                                         
016200     03  FILLER                  PIC X(165)  VALUE SPACE.                 
016300     EJECT                                                                
016400 01  W001-RUBRIK1.                                                        
016500     03  FILLER                  PIC X(2)    VALUE SPACE.                 
016600     03  FILLER                  PIC X(22)                                
016700                                 VALUE 'VOLVO PARTS'.                     
016800     03  FILLER                  PIC X(18)                                
016900                                 VALUE 'W23662-001'.                      
017000     03  FILLER                  PIC X(24)                                
017100                               VALUE 'DELAYED DELIVERIES  '.              
017200     03  FILLER                  PIC X(06)   VALUE SPACE.                 
017300     03  FILLER                  PIC X(7)                                 
017400                                 VALUE 'DATE '.                           
017500     03  W001-DATUM              PIC XXBXXBXX.                            
017600     03  FILLER                  PIC X(2)    VALUE SPACE.                 
017700     03  FILLER                  PIC X(4)                                 
017800                                 VALUE 'PAGE'.                            
017900     03  W001-SID                PIC Z(4)9.                               
018000     03  FILLER                  PIC X(60)   VALUE SPACE.                 
018100                                                                          
018200 01  W001-RUBRIK2.                                                        
018300     03  FILLER                  PIC X(2)  VALUE SPACE.                   
018400     03  FILLER                  PIC X(11) VALUE 'PROCURER   '.           
018500     03  W001-IDANSK             PIC Z(2)9 VALUE ZERO.                    
018600     03  FILLER                  PIC X(2)  VALUE SPACE.                   
018700     03  FILLER                  PIC X(11) VALUE 'SUPPLIER  '.            
018800     03  W001-IDLEVNR            PIC X(5)  VALUE SPACE.                   
018900                                                                          
019000 01  W001-RUBRIK3.                                                        
019100     03  FILLER                  PIC X(42) VALUE SPACE.                   
019200     03  FILLER                  PIC X(12)                                
019300                                 VALUE 'DELAYED DEL.'.                    
019400     03  FILLER                  PIC X(2)  VALUE SPACE.                   
019500     03  FILLER                  PIC X(12)                                
019600                                 VALUE 'DELIV. INFO.'.                    
019700     03  FILLER                  PIC X(2)  VALUE SPACE.                   
020100     03  FILLER                  PIC X(18)                                
020200                                 VALUE 'LAST DELIVERY     '.              
020300     03  FILLER                  PIC X(24) VALUE SPACE.                   
020500                                                                          
020600 01  W001-RUBRIK4.                                                        
020700     03  FILLER                  PIC X(2)  VALUE SPACE.                   
020800     03  FILLER                  PIC X(2)  VALUE 'PR'.                    
020900     03  FILLER                  PIC X(1)  VALUE SPACE.                   
021000     03  FILLER                  PIC X(9)  VALUE '  PART.NO'.             
021100     03  FILLER                  PIC X(2)  VALUE SPACE.                   
021200     03  FILLER                  PIC X(25) VALUE 'DESCRIPTION'.           
021300     03  FILLER                  PIC X(1)  VALUE SPACE.                   
021400     03  FILLER                  PIC X(7)  VALUE 'DEL. W.'.               
021500     03  FILLER                  PIC X(2)  VALUE SPACE.                   
021600     03  FILLER                  PIC X(3)  VALUE 'QTY'.                   
021700     03  FILLER                  PIC X(2)  VALUE SPACE.                   
021800     03  FILLER                  PIC X(7)  VALUE 'DEL. W.'.               
021900     03  FILLER                  PIC X(2)  VALUE SPACE.                   
022000     03  FILLER                  PIC X(3)  VALUE 'QTY'.                   
022100     03  FILLER                  PIC X(2)  VALUE SPACE.                   
022700     03  FILLER                  PIC X(5)  VALUE 'DATE '.                 
022800     03  FILLER                  PIC X(2)  VALUE SPACE.                   
022900     03  FILLER                  PIC X(7)  VALUE 'DES.NOT'.               
024200                                                                          
024300 01  W001-RUBRIK5.                                                        
024400     03  FILLER                  PIC X(07) VALUE SPACE.                   
024500     03  W001-BELEVART           PIC X(30) VALUE SPACE.                   
024600     03  FILLER                  PIC X(04) VALUE SPACE.                   
024700     03  FILLER                  PIC X(16)                                
024800                                 VALUE 'NEXT DELIVERY '.                  
024900     EJECT                                                                
025000 01  W001-DETALJ1.                                                        
025100     03  FILLER                  PIC X(3)  VALUE SPACE.                   
025200     03  W001-KDAVRPRIO          PIC Z     VALUE ZERO.                    
025300     03  FILLER                  PIC X(1)  VALUE SPACE.                   
025400     03  W001-IDARTNR            PIC Z(9)  VALUE ZERO.                    
025500     03  FILLER                  PIC X(2)  VALUE SPACE.                   
025600     03  W001-BEART              PIC X(25) VALUE SPACE.                   
025700     03  FILLER                  PIC X(1)  VALUE SPACE.                   
025800     03  W001-TIAVROP-AVS-FORS   PIC Z(4)  VALUE ZERO.                    
025900     03  FILLER                  PIC X(1)  VALUE SPACE.                   
026000     03  W001-KVAVROP-FORS       PIC Z(7)  VALUE ZERO.                    
026100     03  FILLER                  PIC X(2)  VALUE SPACE.                   
026200     03  W001-TILEVBSK-AVS       PIC Z(4)  VALUE ZERO.                    
026300     03  FILLER                  PIC X(1)  VALUE SPACE.                   
026400     03  W001-KVAVIS-BSKKVAR     PIC Z(7)  VALUE ZERO.                    
026500     03  FILLER                  PIC X(2)  VALUE SPACE.                   
027000     03  W001-TIAVSDAT           PIC Z(6)  VALUE ZERO.                    
027100     03  FILLER                  PIC X(1)  VALUE SPACE.                   
027200     03  W001-IDAVINR            PIC Z(7)  VALUE ZERO.                    
028600                                                                          
028700 01  W001-DETALJ2.                                                        
028800     03  FILLER                  PIC X(39) VALUE SPACE.                   
028900     03  W001-TIAVROP-AVS-NAESTA PIC Z(6)9 VALUE ZERO.                    
029000     03  FILLER                  PIC X(1)  VALUE SPACE.                   
029100     03  W001-KVAVROP-NAESTA     PIC Z(6)9 VALUE ZERO.                    
029200     EJECT                                                                
029300 01  MEM-001.                                                             
029400     03  FILLER                  PIC X(80) VALUE ')SEND '.                
029500 01  MEM-002.                                                             
029600     03  FILLER                  PIC X(80)                                
029700                           VALUE 'TITLE LEV.FÖRS.ART.'.                   
029800 01  MEM-003.                                                             
029900     03  FILLER                  PIC X(80) VALUE 'OPTION FORCE'.          
030000 01  MEM-004.                                                             
030100     03  FILLER                  PIC X(05) VALUE 'DEST '.                 
030200     03  MEM-IDMAIL              PIC X(60) VALUE SPACE.                   
030300 01  MEM-005.                                                             
030400     03  FILLER                  PIC X(05) VALUE 'MEMO '.                 
030500 01  MEM-006.                                                             
030600     03  FILLER                  PIC X(05) VALUE ')END '.                 
030700 01  MEM-007.                                                             
030800     03  FILLER                  PIC X(20) VALUE 'LINESIZE 120'.          
030900     EJECT                                                                
031000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031100*                                                                         
031200                                                                          
031300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031400     SKIP3                                                                
031500 01  NYCKLAR-TILL-DLI.                                                    
031600     03  W-KDARBTYP-X.                                                    
031700         05  W-KDARBTYP          PIC X(08)    VALUE 'ANSK'.               
031800     03  W-IDPERSON-X.                                                    
031900         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
032000     SKIP2                                                                
032100*    --- STATUS-KOD FRÅN IMS                                              
032200 01  STATUS-WS                   PIC XX.                                  
032300     88  SEGMENT-FINNS                       VALUE '  '.                  
032400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032600     SKIP2                                                                
032700 01  GODK-STATUSKODER.                                                    
032800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032900     SKIP3                                                                
033000 01  SSA1                        PIC X(64).                               
033100 01  SSA2                        PIC X(64).                               
033200     EJECT                                                                
033300*    --- IMS FUNKTIONSKODER                                               
033400*01  -COPY W0003                                                          
033500     EJECT                                                                
033600*    ---  DLI INPUT-OUTPUT AREA                                           
033700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
033800 01  DLI-IO-WDP311.                                                       
033900*    03  -COPY WDP311                                                     
034000     EJECT                                                                
034100 LINKAGE SECTION.                                                         
034200                                                                          
034300                                                                          
034400*01  -COPY W0008  -PRE WDP3-                                              
034500     05  FILLER                  PIC X.                                   
034600     EJECT                                                                
034700 PROCEDURE DIVISION  USING WDP3-PCB.                                      
034800 MAIN SECTION.                                                            
034900     ENTRY 'DLITCBL' USING WDP3-PCB.                                      
035000                                                                          
035100                                                                          
035200     PERFORM A-INIT                                                       
035300                                                                          
035400     PERFORM S01-LAES-W23660                                              
035500     PERFORM S02-LAES-W23661                                              
035600     PERFORM UNTIL END-OF-W23660                                          
035700                                                                          
035800       MOVE JA           TO NY-ANSK-LEV                                   
035900       MOVE IN60-IDANSK  TO SPAR-IDANSK                                   
036000       MOVE SPAR-IDLEVNR TO OLD-IDLEVNR                                   
036100       MOVE IN60-IDLEVNR TO SPAR-IDLEVNR                                  
036200                                                                          
036300* ---- TILLS NY ANSKAFFARE ELLER NY LEVERANTÖR                            
036400       PERFORM UNTIL (IN60-IDANSK  NOT = SPAR-IDANSK OR                   
036500                      IN60-IDLEVNR NOT = SPAR-IDLEVNR) OR                 
036600                      END-OF-W23660                                       
036700                                                                          
036800         MOVE JA           TO NY-ARTIKEL                                  
036900         MOVE IN60-IDARTNR TO SPAR-IDARTNR                                
037000                                                                          
037100* ------ TILLS NY ANSKAFFARE, NY LEVERANTÖR ELLER NY ARTIKEL              
037200         PERFORM UNTIL (IN60-IDANSK  NOT = SPAR-IDANSK   OR               
037300                        IN60-IDLEVNR NOT = SPAR-IDLEVNR  OR               
037400                        IN60-IDARTNR NOT = SPAR-IDARTNR) OR               
037500                        END-OF-W23660                                     
037600                                                                          
037700           IF IN60-IDPTYP = 'SEN'                                         
037800             IF IN60-IDARTNR = IN61-IDARTNR AND                           
037900                IN60-IDLEVNR = IN61-IDLEVNR                               
038000                                                                          
038100               PERFORM B-RAD-MED-SEN-INLEV-O-LEVBESK                      
038200               PERFORM S01-LAES-W23660                                    
038300               PERFORM S02-LAES-W23661                                    
038400             ELSE                                                         
038500* ------------ LEVERANSBESKED SLUT (SAKNAS)                               
038600               PERFORM C-RAD-UTAN-LEVBESK                                 
038700               PERFORM S01-LAES-W23660                                    
038800             END-IF                                                       
038900           ELSE                                                           
039000* ---------- IDPTYP = 'NST'                                               
039100* ---------- SISTA POSTEN FÖR ARTIKELN/LEVERANTÖREN PÅ STYRFILEN          
039200             IF IN60-IDARTNR = IN61-IDARTNR AND                           
039300                IN60-IDLEVNR = IN61-IDLEVNR                               
039400                                                                          
039500* ------------ SKRIV UT LEVERANSBESK INNAN RADEN NÄSTA INLEVERANS         
039600               PERFORM UNTIL IN60-IDARTNR NOT = IN61-IDARTNR OR           
039700                             IN60-IDLEVNR NOT = IN61-IDLEVNR OR           
039800                             END-OF-W23661                                
039900                 PERFORM D-RAD-UTAN-SEN-INLEV                             
040000                 PERFORM S02-LAES-W23661                                  
040100               END-PERFORM                                                
040200             END-IF                                                       
040300             MOVE JA TO NAESTA-INLEV                                      
040400             PERFORM E-RAD-MED-NAESTA-INLEV                               
040500             PERFORM S01-LAES-W23660                                      
040600                                                                          
040700           END-IF                                                         
040800         END-PERFORM                                                      
040900       END-PERFORM                                                        
041000                                                                          
041100     END-PERFORM                                                          
041200                                                                          
041300     PERFORM Z-FINIT                                                      
041400                                                                          
041500     MOVE ZERO TO RETURN-CODE                                             
041600     GOBACK                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 A-INIT SECTION.                                                          
042000                                                                          
042100     OPEN INPUT  W23660                                                   
042200                 W23661                                                   
042300                                                                          
042400     OPEN OUTPUT W23662-001                                               
042500                 W23663                                                   
042600                                                                          
042700     ACCEPT DAGENS-DATUM FROM DATE                                        
042800     MOVE DAGENS-DATUM TO W001-DATUM                                      
042900     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
043000     .                                                                    
043100     EJECT                                                                
043200 B-RAD-MED-SEN-INLEV-O-LEVBESK SECTION.                                   
043300                                                                          
043400     IF NY-ARTIKEL = JA                                                   
043500       PERFORM S10-FYLL-I-ARTIKELUPPGIFTER                                
043600     ELSE                                                                 
043700       PERFORM S11-RENSA-ARTIKELUPPGIFTER                                 
043800     END-IF                                                               
043900                                                                          
044000     MOVE IN60-TIAVROP-AVS-FORSENAT TO W001-TIAVROP-AVS-FORS              
044100     MOVE IN60-KVAVROP-FORSENAT     TO W001-KVAVROP-FORS                  
044200     MOVE IN61-TILEVBSK-AVS         TO W001-TILEVBSK-AVS                  
044300     MOVE IN61-KVAVIS-BSKKVAR       TO W001-KVAVIS-BSKKVAR                
044400     MOVE IN60-BELEVART             TO W001-BELEVART                      
044500                                                                          
044600     MOVE W001-DETALJ1 TO W001-RAD                                        
044700     PERFORM S21-SKRIV-W23662-001                                         
044800     .                                                                    
044900     EJECT                                                                
045000 C-RAD-UTAN-LEVBESK SECTION.                                              
045100                                                                          
045200     IF NY-ARTIKEL = JA                                                   
045300       PERFORM S10-FYLL-I-ARTIKELUPPGIFTER                                
045400     ELSE                                                                 
045500       PERFORM S11-RENSA-ARTIKELUPPGIFTER                                 
045600     END-IF                                                               
045700                                                                          
045800     MOVE IN60-TIAVROP-AVS-FORSENAT TO W001-TIAVROP-AVS-FORS              
045900     MOVE IN60-KVAVROP-FORSENAT     TO W001-KVAVROP-FORS                  
046000     MOVE ZERO                      TO W001-TILEVBSK-AVS                  
046100                                       W001-KVAVIS-BSKKVAR                
046200     MOVE IN60-BELEVART             TO W001-BELEVART                      
046300                                                                          
046400     MOVE W001-DETALJ1 TO W001-RAD                                        
046500     PERFORM S21-SKRIV-W23662-001                                         
046600     .                                                                    
046700     EJECT                                                                
046800 D-RAD-UTAN-SEN-INLEV SECTION.                                            
046900                                                                          
047000     IF NY-ARTIKEL = JA                                                   
047100       PERFORM S10-FYLL-I-ARTIKELUPPGIFTER                                
047200     ELSE                                                                 
047300       PERFORM S11-RENSA-ARTIKELUPPGIFTER                                 
047400     END-IF                                                               
047500                                                                          
047600     MOVE IN61-TILEVBSK-AVS   TO W001-TILEVBSK-AVS                        
047700     MOVE IN61-KVAVIS-BSKKVAR TO W001-KVAVIS-BSKKVAR                      
047800     MOVE ZERO                TO W001-TIAVROP-AVS-FORS                    
047900                                 W001-KVAVROP-FORS                        
048000                                                                          
048100     MOVE W001-DETALJ1 TO W001-RAD                                        
048200     PERFORM S21-SKRIV-W23662-001                                         
048300     .                                                                    
048400     EJECT                                                                
048500 E-RAD-MED-NAESTA-INLEV SECTION.                                          
048600                                                                          
048700     MOVE IN60-TIAVROP-AVS-NAESTA TO W001-TIAVROP-AVS-NAESTA              
048800     MOVE IN60-KVAVROP-NAESTA     TO W001-KVAVROP-NAESTA                  
048900     MOVE IN60-BELEVART           TO W001-BELEVART                        
049000                                                                          
049100     MOVE W001-RUBRIK5 TO W001-RAD                                        
049200     PERFORM S21-SKRIV-W23662-001                                         
049300     MOVE W001-DETALJ2 TO W001-RAD                                        
049400     PERFORM S21-SKRIV-W23662-001                                         
049500                                                                          
049600     MOVE NEJ TO NAESTA-INLEV                                             
049700     .                                                                    
049800     EJECT                                                                
049900 Z-FINIT SECTION.                                                         
050000                                                                          
050100     MOVE MEM-006  TO UT-AREA                                             
050200     PERFORM S12-SKRIV-UTPOST-W23663                                      
050300                                                                          
050400     CLOSE W23660                                                         
050500           W23661                                                         
050600           W23662-001                                                     
050700           W23663                                                         
050800                                                                          
050900     MOVE 'S' TO POSTSUM-OPKOD                                            
051000     CALL POSTSUM USING POSTSUM-PARM                                      
051100     .                                                                    
051200     EJECT                                                                
051300 S01-LAES-W23660 SECTION.                                                 
051400                                                                          
051500     READ W23660 INTO IN60-AREA                                           
051600     AT END                                                               
051700        MOVE SPAR-IDANSK  TO IN60-IDANSK                                  
051800        MOVE SPAR-IDLEVNR TO IN60-IDLEVNR                                 
051900        MOVE 999999999    TO IN60-IDARTNR                                 
052000        SET END-OF-W23660 TO TRUE                                         
052100                                                                          
052200     NOT AT END                                                           
052300        MOVE 'W23660'    TO POSTSUM-FDNAMN                                
052400        MOVE 'W23662D1'  TO POSTSUM-DDNAMN2                               
052500        MOVE IN60-IDPTYP TO POSTSUM-TRANSTYP                              
052600        CALL POSTSUM USING POSTSUM-PARM                                   
052700     END-READ                                                             
052800     .                                                                    
052900     EJECT                                                                
053000 S02-LAES-W23661 SECTION.                                                 
053100                                                                          
053200     READ W23661 INTO IN61-AREA                                           
053300     AT END                                                               
053400        MOVE 999       TO IN61-IDANSK                                     
053500        MOVE '99999'   TO IN61-IDLEVNR                                    
053600        MOVE 999999999 TO IN61-IDARTNR                                    
053700        SET END-OF-W23661 TO TRUE                                         
053800                                                                          
053900     NOT AT END                                                           
054000        MOVE 'W23661'   TO POSTSUM-FDNAMN                                 
054100        MOVE 'W23662D2' TO POSTSUM-DDNAMN2                                
054200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
054300        CALL POSTSUM USING POSTSUM-PARM                                   
054400     END-READ                                                             
054500     .                                                                    
054600     EJECT                                                                
054700 S10-FYLL-I-ARTIKELUPPGIFTER SECTION.                                     
054800                                                                          
054900     MOVE IN60-KDAVRPRIO      TO W001-KDAVRPRIO                           
055000     MOVE IN60-IDARTNR        TO W001-IDARTNR                             
055100     MOVE IN60-BEART          TO W001-BEART                               
055400     MOVE IN60-TIAVSDAT       TO W001-TIAVSDAT                            
055500     MOVE IN60-IDAVINR        TO W001-IDAVINR                             
056200     .                                                                    
056300     EJECT                                                                
056400 S11-RENSA-ARTIKELUPPGIFTER SECTION.                                      
056500                                                                          
056600     MOVE SPACE TO W001-BEART                                             
056700     MOVE ZERO  TO W001-KDAVRPRIO                                         
056800                   W001-IDARTNR                                           
057100                   W001-TIAVSDAT                                          
057200                   W001-IDAVINR                                           
057900     .                                                                    
058000     EJECT                                                                
058100 S12-SKRIV-UTPOST-W23663  SECTION.                                        
058200                                                                          
058300     WRITE UT-POST FROM UT-AREA                                           
058400     MOVE 'W23663'   TO POSTSUM-FDNAMN                                    
058500     MOVE 'W23662D4' TO POSTSUM-DDNAMN2                                   
058600     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
058700     CALL POSTSUM USING POSTSUM-PARM                                      
058800     .                                                                    
058900     EJECT                                                                
059000 S21-SKRIV-W23662-001 SECTION.                                            
059100                                                                          
059200     IF NY-ANSK-LEV = JA                                                  
059300       MOVE NEJ  TO NY-ANSK-LEV                                           
059400       MOVE +999 TO W001-ANTAL-RADER                                      
059500     ELSE                                                                 
059600       IF NY-ARTIKEL = JA                                                 
059700*------- MINSTA PLATSEN EN ARTIKEL TAR ÄR 3 RADER                         
059800*------- KOLLA ATT DE FÅR PLATS PÅ SIDAN, ANNARS NY SIDA                  
059900         IF (W001-ANTAL-RADER + 3) > W001-MAX-RADER-PER-SIDA              
060000           MOVE +999 TO W001-ANTAL-RADER                                  
060100         END-IF                                                           
060200       ELSE                                                               
060300         IF NAESTA-INLEV = JA                                             
060400*--------- NÄSTA INLEV TAR 2 RADER                                        
060500*--------- KOLLA ATT DE FÅR PLATS PÅ SIDAN, ANNARS NY SIDA                
060600           IF (W001-ANTAL-RADER + 2) > W001-MAX-RADER-PER-SIDA            
060700             MOVE +999 TO W001-ANTAL-RADER                                
060800           END-IF                                                         
060900         END-IF                                                           
061000       END-IF                                                             
061100     END-IF                                                               
061200                                                                          
061300     IF NY-ARTIKEL = JA                                                   
061400       MOVE 2 TO W001-SKIP                                                
061500     ELSE                                                                 
061600       MOVE 1 TO W001-SKIP                                                
061700     END-IF                                                               
061800                                                                          
061900     IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA                        
062000       PERFORM S22-SKRIV-RUBRIK1                                          
062100       PERFORM S23-SKRIV-RUBRIK2                                          
062200       PERFORM S24-SKRIV-RUBRIK3-4                                        
062300     END-IF                                                               
062400                                                                          
062500     WRITE W23662-001-RAD FROM W001-RAD AFTER W001-SKIP                   
062610     MOVE W001-RAD         TO UT-AREA                                     
062700     PERFORM S12-SKRIV-UTPOST-W23663                                      
062800                                                                          
062900     MOVE SPACE TO W001-RAD                                               
063000     IF NY-ARTIKEL = JA                                                   
063100       ADD +2   TO W001-ANTAL-RADER                                       
063200     ELSE                                                                 
063300       ADD +1   TO W001-ANTAL-RADER                                       
063400     END-IF                                                               
063500                                                                          
063600     MOVE NEJ TO NY-ARTIKEL                                               
063700                 NAESTA-INLEV                                             
063800     .                                                                    
063900     EJECT                                                                
064000 S22-SKRIV-RUBRIK1 SECTION.                                               
064100                                                                          
064200     ADD +1 TO W001-SIDRAKNARE                                            
064300     MOVE W001-SIDRAKNARE   TO W001-SID                                   
064400     MOVE IN60-IDANSK       TO W001-IDANSK                                
064500     MOVE IN60-IDLEVNR      TO W001-IDLEVNR                               
064600                                                                          
064700     PERFORM S22A-SKAPA-MAIL-DEST                                         
064800                                                                          
064900     WRITE W23662-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
065010     MOVE W001-RUBRIK1        TO UT-AREA                                  
065100     PERFORM S12-SKRIV-UTPOST-W23663                                      
065200                                                                          
065300     MOVE +2 TO W001-ANTAL-RADER                                          
065400                                                                          
065500     MOVE +2 TO W001-SKIP                                                 
065600     .                                                                    
065700     EJECT                                                                
065800 S22A-SKAPA-MAIL-DEST SECTION.                                            
065900                                                                          
066000     IF SPAR-IDANSK (1:2) NOT = OLD-IDANSK (1:2)                          
066100        IF OLD-IDANSK NOT = 999                                           
066200           MOVE MEM-006  TO UT-AREA                                       
066300           PERFORM S12-SKRIV-UTPOST-W23663                                
066400        END-IF                                                            
066500        MOVE SPAR-IDANSK TO OLD-IDANSK                                    
066600        MOVE MEM-001  TO UT-AREA                                          
066700        PERFORM S12-SKRIV-UTPOST-W23663                                   
066800        MOVE MEM-002  TO UT-AREA                                          
066900        PERFORM S12-SKRIV-UTPOST-W23663                                   
067000        MOVE MEM-003  TO UT-AREA                                          
067100        PERFORM S12-SKRIV-UTPOST-W23663                                   
067200        MOVE MEM-007  TO UT-AREA                                          
067300        PERFORM S12-SKRIV-UTPOST-W23663                                   
067400                                                                          
067500        PERFORM S30-HAMTA-NAMN                                            
067600                                                                          
067700        MOVE MEM-004  TO UT-AREA                                          
067800        PERFORM S12-SKRIV-UTPOST-W23663                                   
067900        MOVE MEM-005  TO UT-AREA                                          
068000        PERFORM S12-SKRIV-UTPOST-W23663                                   
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400 S23-SKRIV-RUBRIK2 SECTION.                                               
068500                                                                          
068600     WRITE W23662-001-RAD FROM W001-RUBRIK2 AFTER 2                       
068700     MOVE W001-RUBRIK2         TO UT-AREA                                 
068800     PERFORM S12-SKRIV-UTPOST-W23663                                      
068900                                                                          
069000     ADD +2 TO W001-ANTAL-RADER                                           
069100                                                                          
069200     MOVE +2 TO W001-SKIP                                                 
069300     .                                                                    
069400     EJECT                                                                
069500 S24-SKRIV-RUBRIK3-4 SECTION.                                             
069600                                                                          
069700     WRITE W23662-001-RAD FROM W001-RUBRIK3 AFTER 1                       
069810     MOVE W001-RUBRIK3         TO UT-AREA                                 
069900     PERFORM S12-SKRIV-UTPOST-W23663                                      
070000                                                                          
070100     WRITE W23662-001-RAD FROM W001-RUBRIK4 AFTER 1                       
070210     MOVE W001-RUBRIK4         TO UT-AREA                                 
070300     PERFORM S12-SKRIV-UTPOST-W23663                                      
070400                                                                          
070500     ADD +3 TO W001-ANTAL-RADER                                           
070600                                                                          
070700     MOVE +2 TO W001-SKIP                                                 
070800     .                                                                    
070900     EJECT                                                                
071000 S30-HAMTA-NAMN SECTION.                                                  
071100                                                                          
071200     MOVE SPAR-IDANSK           TO W-IDPERSON                             
071300     PERFORM IMS-GET-WDP3-ANSKNAMN                                        
071400     IF SEGMENT-FINNS                                                     
071500        MOVE PERS-IDMAIL                   TO MEM-IDMAIL                  
071600     ELSE                                                                 
071710        MOVE 'ALEXANDER.AUGUSTSSON@VOLVOCARS.COM' TO MEM-IDMAIL           
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100* --- IMS SEKTIONER ---                                                   
072200                                                                          
072300 IMS-GET-WDP3-ANSKNAMN SECTION.                                           
072400                                                                          
072500     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
072600          DELIMITED BY SIZE INTO SSA1                                     
072700     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
072800          DELIMITED BY SIZE INTO SSA2                                     
072900     MOVE '  GE' TO GODK-STATUSKODER                                      
073000     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
073100     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
073200     PERFORM IMS-STATUSKONTROLL                                           
073300     .                                                                    
073400     EJECT                                                                
073500 IMS-STATUSKONTROLL SECTION.                                              
073600                                                                          
073700     SET STATUS-IX TO 1                                                   
073800     SEARCH GODK-STATUS                                                   
073900       AT END                                                             
074000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
074100           DELIMITED BY SIZE INTO FELTEXT                                 
074200         DISPLAY FELTEXT                                                  
074300         CALL FELLOG                                                      
074400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
074500         CONTINUE                                                         
074600     END-SEARCH                                                           
074700     .                                                                    
