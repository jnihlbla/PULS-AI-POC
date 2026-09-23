000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6126000.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   97/05/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR UTLISTA PÅ FÖRVÄNTADE FLYGLEVERNSER TILL NDC              
001100*                                                                         
001200* ÄNDRING:                                                                
001300* MINDRE ÄNDRING SÅ RESULTATET BLIR DETSAMMA SOM                          
001400* W6125200   97 11 05    AV JOHAN L                                       
001500*                                                                         
001600* 97-11-26  JOHAN L                                                       
001700* NUMERA ÄVEN TRANSFER MELLAN NDC MED PÅ LISTAN                           
001800*                                                                         
001900* 98-02-23  JOHAN L                                                       
002000* NUMERA ÄVEN BINNED PRIO LINES PÅ LISTAN                                 
002100*                                                                         
002200* 99-03-02 JOHAN L                                                        
002300* JPN OCH AUS SKALL EJ HA "VALUE" I YEN/AUS DOLLAR UTAN I SEK             
002400*                                                                         
002500* 06-08-01 MARKUS                                                         
002600* LISTOR SOM SKAPAS FÖR LDC SKICKAS TILL D&P OCH KAN SES VIA              
002700* DOCUMENT RETRIEVAL PÅ WEBBEN                                            
002800*                                                                         
002900* 07-03-12                                                                
003000* NU SKALL OCKSÅ ANTAL RADER SOM FINNS KVAR PÅ AK VISAS PÅ                
003100* LISTORNA PÅ WEBBEN                                                      
003200*                                                                         
003300* 11-10-27                                                                
003400* PRINT ADDITIONAL LINES(MODE OF TRANSPORT) FOR CHINA                     
003500*                                                                         
003600*                                                                         
003700*                                                                         
003800*    ABENDKODER:                                                          
003900*        U0016 -  . . . .                                                 
004000*        U1000 -  . . . .                                                 
004100*                                                                         
004200                                                                          
004300     SKIP3                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500     SKIP2                                                                
004600 INPUT-OUTPUT SECTION.                                                    
004700                                                                          
004800 FILE-CONTROL.                                                            
004900     SKIP2                                                                
005000*          --- SORTERAD INFIL                                             
005100     SELECT W61256                     ASSIGN TO W61260D1.                
005200     SKIP2                                                                
005300*          --- UTLISTA                                                    
005400     SELECT W61260-001                 ASSIGN TO W61260D2.                
005500     EJECT                                                                
005600 DATA DIVISION.                                                           
005700     SKIP3                                                                
005800 FILE SECTION.                                                            
005900     SKIP3                                                                
006000 FD  W61256                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  -COPY W61256      -L.                                                
006500     SKIP3                                                                
006600 FD  W61260-001                                                           
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900     SKIP2                                                                
007000 01  W61260-001-RAD              PIC X(121).                              
007100     EJECT                                                                
007200 WORKING-STORAGE SECTION.                                                 
007300                                                                          
007400*    -- INDEX IN THE TABLE BELOW.                                         
007500*    -- 1=AIR, 2=BOAT, 3=TRANSFER 4=OTHER 5=TOTAL, SUM OF 1-4             
007600 01  I                           PIC S9  COMP-3.                          
007700 01  MAX-I                       PIC S9  COMP-3    VALUE 5.               
007800*    -- CONSTANTS FOR VALUES OF I                                         
007900 01  IA                          PIC S9  COMP-3    VALUE 1.               
008000 01  IB                          PIC S9  COMP-3    VALUE 2.               
008100 01  IT                          PIC S9  COMP-3    VALUE 3.               
008200 01  IZ                          PIC S9  COMP-3    VALUE 4.               
008300                                                                          
008400 77  WS-AIR                      PIC X(1)   VALUE 'A'.                    
008500 77  WS-BOAT                     PIC X(1)   VALUE 'B'.                    
008600 77  WS-TRANSFERS                PIC X(1)   VALUE 'T'.                    
008700 77  WS-OTHERS                   PIC X(1)   VALUE 'Z'.                    
008800 77  WS-TOTALS                   PIC X(1)   VALUE ' '.                    
008900                                                                          
009000 01  SPAR-IDDC                   PIC XX      VALUE SPACE.                 
009100 01  TALLYS.                                                              
009200     03 SAVES OCCURS 5 TIMES.                                             
009300       05 AK-LINES               PIC S9(5)       COMP-3.                  
009400       05 AK-VALUE               PIC S9(7)V99    COMP-3.                  
009500       05 BO-VALUE               PIC S9(7)V99    COMP-3.                  
009600       05 TIME-TOT               PIC S9(5)V9     COMP-3.                  
009700       05 TIMEPRIO               PIC S9(5)V9     COMP-3.                  
009800       05 BINNED-L               PIC S9(5)       COMP-3.                  
009900       05 BINNED-V               PIC S9(7)V99    COMP-3.                  
010000       05 TIME-LINE-PRIO         PIC S9(5)       COMP-3.                  
010100                                                                          
010200 01  TEMP-VALUES.                                                         
010300     03 FILLER.                                                           
010400        05 TEMP-LINE-AK          PIC 9 COMP-3   VALUE ZERO.               
010500        05 TEMP-LINE-BI          PIC 9 COMP-3   VALUE ZERO.               
010600        05 TEMP-TIME-LINE-PRIO   PIC 9 COMP-3   VALUE ZERO.               
010700     03 FILLER.                                                           
010800        05 TEMP-VALUE-AK         PIC S9(7)V99 COMP-3   VALUE ZERO.        
010900        05 TEMP-VALUE-BO         PIC S9(7)V99 COMP-3   VALUE ZERO.        
011000        05 TEMP-VALUE-BI         PIC S9(7)V99 COMP-3   VALUE ZERO.        
011100     03 FILLER.                                                           
011200        05 TEMP-TOT-TIME         PIC S9(4)V9 COMP-3   VALUE ZERO.         
011300        05 TEMP-PRIO-TIME        PIC S9(4)V9 COMP-3   VALUE ZERO.         
011400                                                                          
011500 01  TEMP-DECTAL                 PIC Z(6)9.99.                            
011600 01  TEMP-HELTAL                 PIC Z(9)9.                               
011700 01  TEMP-DECTAL2                PIC Z(4)9.9.                             
011800                                                                          
011900 01 NDC-TIDER.                                                            
012000     03 NDC-ARBTID-START         PIC X(4).                                
012100     03 NDC-ARBTID-SLUT          PIC X(4).                                
012200                                                                          
012300 01  FL-OK-POST                  PIC X       VALUE 'J'.                   
012400 01  KD-DIFF-ARB-DAG             PIC 999     VALUE 501.                   
012500 01  KD-NEXT-ARB-DAG             PIC 999     VALUE 502.                   
012600 01  KD-LAST-ARB-DAG             PIC 999     VALUE 503.                   
012700                                                                          
012800 77  IDPGM                       PIC X(8)    VALUE 'W6126000'.            
012900 77  JA                          PIC X       VALUE 'J'.                   
013000*77  YES                         PIC X       VALUE 'Y'.                   
013100 77  NEJ                         PIC X       VALUE 'N'.                   
013200*77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
013300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
013400 77  KDRC-DISPLAY                PIC Z(5).                                
013500                                                                          
013600 77  W61256-EOF-SW               PIC X       VALUE 'N'.                   
013700     88  END-OF-W61256                       VALUE 'J'.                   
013800     EJECT                                                                
013900*                                                                         
014000                                                                          
014100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014200 01  FILLER REDEFINES DAGENS-DATUM.                                       
014300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
014600 01  WS-YYMMDDHHMM.                                                       
014700     03 WS-YYMMDD                PIC  9(6).                               
014800     03 WS-TIME                  PIC  9(4).                               
014900                                                                          
015000 01  WS-HHMMSSTH.                                                         
015100     03 WS-HHMM                  PIC  9(4).                               
015200     03 WS-SSTH                  PIC  9(4).                               
015300                                                                          
015400     EJECT                                                                
015500 01  DYNAMISKA-SUBPROGRAM.                                                
015600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015900     03  W612TIME                PIC X(8)    VALUE 'W612TIME'.            
016000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
016300     SKIP2                                                                
016400*    --- PARAMETRAR TILL ABEND                                            
016500                                                                          
016600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
016800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
017000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
017100     SKIP2                                                                
017200 01  FELTEXT.                                                             
017300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
017400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017500     EJECT                                                                
017600*    --- PARAMETRAR TILL DATKORT                                          
017700*                                                                         
017800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61260'.              
017900     SKIP2                                                                
018000 01  DATUMKORT-ID                PIC X(6)    VALUE '000000'.              
018100     SKIP2                                                                
018200*01  -COPY WDATKORT                                                       
018300     EJECT                                                                
018400*    --- PARAMETRAR TILL POSTSUM                                          
018500*                                                                         
018600*01  -COPY W0005   -PRE  POSTSUM-                                         
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL W612TIME                                         
018900*                                                                         
019000*01  -COPY W612TID     -PRE TIME-                                         
019100 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
019200*01  -COPY WZ01SEND                                                       
019300     EJECT                                                                
019400     SKIP2                                                                
019500*      --- VALID IDDC CODES                                               
019600*                                                                         
019700*01  -COPY WWDC99                                                         
019800       EJECT                                                              
019900 01  HDR-AREA.                                                            
020000*   03  -COPY WZ01REQU -PRE HDR-                                          
020100*   03  -COPY WZ04HDR                                                     
020200*                                                                         
020300 01  FILLER                      PIC X(24) VALUE 'DAP-LINE-AREA'.         
020400 01  DAP-LINE-AREA.                                                       
020500*    03 -COPY W612601 -PRE DAP-LINE-                                      
020600     EJECT                                                                
020700                                                                          
020800     EJECT                                                                
020900*01  -COPY WWDIST35                                                       
021000                                                                          
021100     EJECT                                                                
021200 01  IN-AREA-START               PIC X(24)   VALUE                        
021300                                 'IN-AREA-START  '.                       
021400     SKIP2                                                                
021500     EJECT                                                                
021600                                                                          
021700*01  AREA -COPY W61256     -PRE IN-                                       
021800     EJECT                                                                
021900 01  W001-AREA-START             PIC X(24)   VALUE                        
022000                                 'W001-AREA-START  '.                     
022100     SKIP2                                                                
022200 01  W001-HJALPAREOR.                                                     
022300*                                                                         
022400     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 3.                
022500     03  W001-ANTAL-RADER                                                 
022600                                 PIC 9(3)    VALUE 999.                   
022700     03  W001-MAX-RADER-PER-SIDA                                          
022800                                 PIC 9(3)    VALUE 42.                    
022900     03  W001-MAX-POSITIONER-PER-RAD                                      
023000                                 PIC 9(3)    VALUE 120.                   
023100     03  W001-LISTNR             PIC X(11)   VALUE 'W61260-001'.          
023200     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
023300     EJECT                                                                
023400 01  W001-RAD.                                                            
023500*                                                                         
023600     03  FILLER                  PIC X(121)  VALUE SPACE.                 
023700     EJECT                                                                
023800 01  W001-RUBRIK1.                                                        
023900*                                                                         
024000     03  FILLER                  PIC X(3) VALUE SPACE.                    
024100     03  FILLER                  PIC X(21)                                
024200                                VALUE 'VOLVO CAR CORP. PARTS'.            
024300     03  FILLER                  PIC X(12)                                
024400                                 VALUE 'W61260-001'.                      
024500     03  FILLER                  PIC X(30)                                
024600              VALUE '    DAILY AK FOLLOW-UP REFILL.'.                     
024700     03  FILLER                  PIC X(10) VALUE SPACE.                   
024800     03  FILLER                  PIC X(4) VALUE 'DC '.                    
024900     03  RUBRIK-DC               PIC XX.                                  
025000     03  FILLER                  PIC X(9) VALUE SPACE.                    
025100     03  W001-DATUM              PIC XXBXXBXX.                            
025200     03  FILLER                  PIC X(4) VALUE SPACE.                    
025300     03  FILLER                  PIC X(4)                                 
025400                                 VALUE 'PAGE'.                            
025500     03  W001-SID                PIC Z(4)9.                               
025600     EJECT                                                                
025700 01  W001-RUBRIK2.                                                        
025800*                                                                         
025900     03  FILLER                  PIC X(19) VALUE SPACE.                   
026000     03  FILLER                  PIC XX    VALUE 'AK'.                    
026100     03  FILLER                  PIC X(11)  VALUE SPACE.                  
026200     03  FILLER                  PIC XX    VALUE 'AK'.                    
026300     03  FILLER                  PIC X(11)  VALUE SPACE.                  
026400     03  FILLER                  PIC XX    VALUE 'BO'.                    
026500     03  FILLER                  PIC X(8)  VALUE SPACE.                   
026600     03  FILLER                  PIC X(17)                                
026700                                 VALUE 'TIME   IN    DAYS'.               
026800     03  FILLER                  PIC X(10)  VALUE SPACE.                  
026900     03  FILLER                  PIC X(6)  VALUE 'BINNED'.                
027000     03  FILLER                  PIC X(8)  VALUE SPACE.                   
027100     03  FILLER                  PIC X(6)  VALUE ' PRIO '.                
027200     03  FILLER                  PIC X(6)  VALUE SPACE.                   
027300     03  FILLER                  PIC X(7)  VALUE 'BINNED'.                
027400     EJECT                                                                
027500 01  W001-RUBRIK3.                                                        
027600*                                                                         
027700     03  FILLER                  PIC X(18) VALUE SPACE.                   
027800     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
027900     03  FILLER                  PIC X(8)  VALUE SPACE.                   
028000     03  FILLER                  PIC X(5)  VALUE 'VALUE'.                 
028100     03  FILLER                  PIC X(8)  VALUE SPACE.                   
028200     03  FILLER                  PIC X(5)  VALUE 'VALUE'.                 
028300     03  FILLER                  PIC X(7)  VALUE SPACE.                   
028400     03  FILLER                  PIC X(3)  VALUE 'TOT'.                   
028500     03  FILLER                  PIC X(9)  VALUE SPACE.                   
028600     03  FILLER                  PIC X(4)  VALUE 'PRIO'.                  
028700     03  FILLER                  PIC X(11)  VALUE SPACE.                  
028800     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
028900     03  FILLER                  PIC X(8)  VALUE SPACE.                   
029000     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
029100     03  FILLER                  PIC X(8)  VALUE SPACE.                   
029200     03  FILLER                  PIC X(5)  VALUE 'VALUE'.                 
029300     EJECT                                                                
029400 01  W001-DETALJ.                                                         
029500     03 FILLER                   PIC X(9)    VALUE SPACE.                 
029600     03 L-KDREFTYP               PIC X       VALUE SPACE.                 
029700     03 FILLER                   PIC XXX     VALUE SPACE.                 
029800     03 L-AK-LINES               PIC X(10).                               
029900     03 FILLER                   PIC XXX     VALUE SPACE.                 
030000     03 L-AK-VALUE               PIC X(10).                               
030100     03 FILLER                   PIC XXX     VALUE SPACE.                 
030200     03 L-BO-VALUE               PIC X(10).                               
030300     03 FILLER                   PIC XXX     VALUE SPACE.                 
030400     03 L-TIME-TOT               PIC X(10).                               
030500     03 FILLER                   PIC XXX     VALUE SPACE.                 
030600     03 L-TIMEPRIO               PIC X(10).                               
030700     03 FILLER                   PIC XXX     VALUE SPACE.                 
030800     03 L-BINNED-L               PIC X(10).                               
030900     03 FILLER                   PIC XXX     VALUE SPACE.                 
031000     03 L-BINNED-P               PIC X(10).                               
031100     03 FILLER                   PIC XXX     VALUE SPACE.                 
031200     03 L-BINNED-V               PIC X(10).                               
031300     EJECT                                                                
031400                                                                          
031500* REPORT LAYOUT OF DC 11 VIA D&P                                          
031600 01  W002-LINE                   PIC X(93).                               
031700     EJECT                                                                
031800 01  W002-RAD1.                                                           
031900*                                                                         
032000     03  FILLER                  PIC X(26)                                
032100                               VALUE 'VOLVO CAR CUSTOMER SERVICE'.        
032200     03  FILLER                  PIC X(07) VALUE SPACE.                   
032300     03  FILLER                  PIC X(30)                                
032400              VALUE 'REFILL AK DAILY FOLLOW UP  DC '.                     
032500     03  W002-IDDC               PIC XX.                                  
032600     03  FILLER                  PIC X(10) VALUE SPACE.                   
032700     03  FILLER                  PIC X(05) VALUE 'DATE '.                 
032800     03  W002-DATUM              PIC XXBXXBXX.                            
032900     03  FILLER                  PIC X(05) VALUE SPACE.                   
033000     EJECT                                                                
033100 01  W002-RAD2.                                                           
033200*                                                                         
033300     03  FILLER                  PIC X(26) VALUE SPACE.                   
033400     03  FILLER                  PIC X(14) VALUE 'NOT YET BINNED'.        
033500     03  FILLER                  PIC X(04) VALUE SPACE.                   
033600     03  FILLER                  PIC X(12) VALUE 'TOTAL BINNED'.          
033700     03  FILLER                  PIC X(08) VALUE SPACE.                   
033800     03  FILLER                  PIC X(04) VALUE 'PRIO'.                  
033900     03  FILLER                  PIC X(10) VALUE SPACE.                   
034000     03  FILLER                  PIC X(12) VALUE 'TIME IN DAYS'.          
034100     03  FILLER                  PIC X(03) VALUE SPACE.                   
034200     EJECT                                                                
034300 01  W002-RAD3.                                                           
034400*                                                                         
034500     03  FILLER                  PIC X(35) VALUE SPACE.                   
034600     03  FILLER                  PIC X(05) VALUE 'LINES'.                 
034700     03  FILLER                  PIC X(11) VALUE SPACE.                   
034800     03  FILLER                  PIC X(05) VALUE 'LINES'.                 
034900     03  FILLER                  PIC X(07) VALUE SPACE.                   
035000     03  FILLER                  PIC X(05) VALUE 'LINES'.                 
035100     03  FILLER                  PIC X(09) VALUE SPACE.                   
035200     03  FILLER                  PIC X(03) VALUE 'TOT'.                   
035300     03  FILLER                  PIC X(09) VALUE SPACE.                   
035400     03  FILLER                  PIC X(04) VALUE 'PRIO'.                  
035500     EJECT                                                                
035600 01  W002-DETAIL.                                                         
035700     03 W002-KDREFTYP            PIC X(10)   VALUE SPACE.                 
035800     03 FILLER                   PIC X(24)   VALUE SPACE.                 
035900     03 W002-AK-LINES            PIC Z(5)9.                               
036000     03 FILLER                   PIC X(10)   VALUE SPACE.                 
036100     03 W002-BINNED-LINES        PIC Z(5)9.                               
036200     03 FILLER                   PIC X(06)   VALUE SPACE.                 
036300     03 W002-BINNED-PRIO         PIC Z(5)9.                               
036400     03 FILLER                   PIC X(06)   VALUE SPACE.                 
036500     03 W002-TIME-TOT            PIC Z(3)9.9.                             
036600     03 FILLER                   PIC X(07)   VALUE SPACE.                 
036700     03 W002-DAYS-PRIO           PIC Z(3)9.9.                             
036800     EJECT                                                                
036900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
037000*                                                                         
037100     EJECT                                                                
037200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
037300     SKIP3                                                                
037400 01  NYCKLAR-TILL-DLI.                                                    
037500     03  W-IDDC-B6-X.                                                     
037600         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
037700     SKIP2                                                                
037800*    --- STATUS-KOD FRÅN IMS                                              
037900 01  STATUS-WS                   PIC XX.                                  
038000     88  SEGMENT-FINNS                       VALUE '  '.                  
038100     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
038200     SKIP2                                                                
038300 01  GODK-STATUSKODER.                                                    
038400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038500     SKIP3                                                                
038600 01  SSA1                        PIC X(64).                               
038700 01  SSA2                        PIC X(64).                               
038800     EJECT                                                                
038900*    --- IMS FUNKTIONSKODER                                               
039000*01  -COPY W0003                                                          
039100     EJECT                                                                
039200*    ---  DLI INPUT-OUTPUT AREA                                           
039300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
039400 01   DLI-IO-AREA-B601.                                                   
039500*     03  -COPY WDB601                                                    
039600     EJECT                                                                
039700 LINKAGE SECTION.                                                         
039800*01  -COPY W0009   -PRE MSG-                                              
039900 01  DAP-PCB              PIC X.                                          
040000     EJECT                                                                
040100*01  -COPY W0008      -PRE WDB6-                                          
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
040500 MAIN SECTION.                                                            
040600     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
040700                                                                          
040800     PERFORM A-INIT                                                       
040900     PERFORM S01-LAES-W61256                                              
041000     PERFORM UNTIL END-OF-W61256                                          
041100       PERFORM B-IF-NEW-DC-THEN-WRITE                                     
041200       PERFORM C-CHECK-INPOST                                             
041300       PERFORM S01-LAES-W61256                                            
041400     END-PERFORM                                                          
041500                                                                          
041600     PERFORM F-WRITE-LINES-OR-RECORDS                                     
041700     PERFORM Z-FINIT                                                      
041800                                                                          
041900     MOVE ZERO TO RETURN-CODE                                             
042000     GOBACK                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 A-INIT SECTION.                                                          
042400                                                                          
042500     OPEN INPUT  W61256                                                   
042600     OPEN OUTPUT W61260-001                                               
042700                                                                          
042800     INITIALIZE TALLYS                                                    
042900     SKIP2                                                                
043000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
043100     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
043200     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
043300     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
043400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
043500     ACCEPT WS-YYMMDD      FROM DATE                                      
043600     ACCEPT WS-HHMMSSTH    FROM TIME                                      
043700     MOVE   WS-HHMM      TO WS-TIME                                       
043800                                                                          
043900     .                                                                    
044000     EJECT                                                                
044100 B-IF-NEW-DC-THEN-WRITE SECTION.                                          
044200                                                                          
044300     IF SPAR-IDDC = SPACE                                                 
044400       CONTINUE                                                           
044500     ELSE                                                                 
044600       IF IN-SHIST-IDDC = SPAR-IDDC                                       
044700         CONTINUE                                                         
044800       ELSE                                                               
044900         PERFORM F-WRITE-LINES-OR-RECORDS                                 
045000       END-IF                                                             
045100     END-IF                                                               
045200                                                                          
045300     IF SPAR-IDDC NOT = IN-SHIST-IDDC                                     
045400       INITIALIZE TALLYS                                                  
045500*      -- FETCH INFO FOR NEW DC                                           
045600       MOVE IN-SHIST-IDDC TO SPAR-IDDC                                    
045700                             W-IDDC-B6                                    
045800       PERFORM IMS-GU-WDB601                                              
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 C-CHECK-INPOST SECTION.                                                  
046300                                                                          
046400     EVALUATE IN-SHIST-IDPTYP                                             
046500     WHEN '310'                                                           
046600       PERFORM D-310                                                      
046700     WHEN 'R32'                                                           
046800       PERFORM E-R32                                                      
046900     WHEN OTHER                                                           
047000       MOVE 'FEL POSTTYP!!!' TO FELTEXT-STR                               
047100       PERFORM S99-ABEND                                                  
047200     END-EVALUATE                                                         
047300     PERFORM S31-ASSIGN-TALLYS                                            
047400     PERFORM S41-NOLLSTAELL-TEMP-FAELT                                    
047500     .                                                                    
047600     EJECT                                                                
047700 D-310 SECTION.                                                           
047800                                                                          
047900     MOVE 1 TO TEMP-LINE-AK                                               
048000     IF DCS-NDC-PF OR DCS-NDC-OTHERS                                      
048100        COMPUTE TEMP-VALUE-AK ROUNDED =                                   
048200           IN-SHIST-KVAVIS * IN-SHIST-PRARTNTO                            
048300        COMPUTE TEMP-VALUE-BO ROUNDED =                                   
048400           IN-SHIST-KVROS * IN-SHIST-PRARTNTO                             
048500     ELSE                                                                 
048600        COMPUTE TEMP-VALUE-AK ROUNDED =                                   
048700           IN-SHIST-KVAVIS * (IN-SHIST-PRARTNTO / IN-SHIST-PRKURS)        
048800        COMPUTE TEMP-VALUE-BO ROUNDED =                                   
048900           IN-SHIST-KVROS * IN-SHIST-PRAVCOST                             
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 E-R32 SECTION.                                                           
049400                                                                          
049500     MOVE JA TO FL-OK-POST                                                
049600     EVALUATE FALSE                                                       
049700     WHEN IN-SHIST-TIINLMOT > 0                                           
049800       MOVE NEJ TO FL-OK-POST                                             
049900     WHEN IN-SHIST-TIINLMTI > 0                                           
050000       MOVE NEJ TO FL-OK-POST                                             
050100     WHEN IN-SHIST-TIINLINL > 0                                           
050200       MOVE NEJ TO FL-OK-POST                                             
050300     WHEN IN-SHIST-TIINLITI > 0                                           
050400       MOVE NEJ TO FL-OK-POST                                             
050500     END-EVALUATE                                                         
050600                                                                          
050700     IF FL-OK-POST = JA                                                   
050800       MOVE 1 TO TEMP-LINE-BI                                             
050900                                                                          
051000* IN-SHIST-PRAVCOST FÖR KINA ?                                            
051100       IF DCS-NDC-PF OR DCS-NDC-OTHERS                                    
051200         COMPUTE TEMP-VALUE-BI ROUNDED =                                  
051300         IN-SHIST-KVANTMOT * IN-SHIST-PRARTNTO                            
051400       ELSE                                                               
051500         MOVE ZERO TO TEMP-VALUE-BI                                       
051600         IF IN-SHIST-PRKURS > 0                                           
051700           COMPUTE TEMP-VALUE-BI ROUNDED =                                
051800             IN-SHIST-KVANTMOT *                                          
051900             (IN-SHIST-PRARTNTO / IN-SHIST-PRKURS)                        
052000         END-IF                                                           
052100       END-IF                                                             
052200       PERFORM S21-TIDSBERAK                                              
052300     ELSE                                                                 
052400*      OBS! EN R32:A SOM INTE ÄR MOTTAGEN ELLER INLAGD                    
052500       CONTINUE                                                           
052600     END-IF                                                               
052700     .                                                                    
052800                                                                          
052900     EJECT                                                                
053000 F-WRITE-LINES-OR-RECORDS  SECTION.                                       
053100                                                                          
053200*    SEND DATA VIA D&P FOR WEB WAREHOUSES OR PRINT                        
053300*    DATA VIA D&P FOR CDC, DIFFERENT LAYOUT FROM WEB DC'S                 
053400*    TRADITIONAL REPORT FOR OHER WAREHOUSES                               
053500     IF DCS-FLWEBDC = JA OR DCS-CDC                                       
053600                                                                          
053700       PERFORM S50-SEND-OPEN                                              
053800       PERFORM S11-WRITE-DAP-HEADER                                       
053900                                                                          
054000*      CDC & CHINESE DC:S SHOULD HAVE ONE LINE PER TRANSPORT TYPE         
054100*      BUT EUROPEAN LDC:S (AND SDC:S) SHOULD ONLY HAVE ONE                
054200*      LINE WITH TOTAL SUMS FOR ALL TRANSPORT TYPES                       
054300       MOVE SPAR-IDDC TO WS-IDDC                                          
054400       IF NDC-CN OR LDC-CN OR CDC-SE                                      
054500                                                                          
054600         MOVE 1 TO I                                                      
054700         PERFORM UNTIL I > MAX-I                                          
054800*        -- OM INTE ALLT ÄR NOLLOR                                        
054900           IF AK-LINES(I)  > 0                                            
055000           OR BINNED-L(I)  > 0                                            
055100           OR TIME-LINE-PRIO(I) > 0                                       
055200             IF CDC-SE                                                    
055300               PERFORM FB-WRITE-DAP-LINE-CDC                              
055400             ELSE                                                         
055500               PERFORM FA-WRITE-DAP-LINE-INDEX-I                          
055600             END-IF                                                       
055700           END-IF                                                         
055800           ADD 1 TO I                                                     
055900         END-PERFORM                                                      
056000       ELSE                                                               
056100*        -- ONLY TOTALS, VALUES STORED IN HIGHEST TABLE INDEX             
056200         MOVE MAX-I TO I                                                  
056300         PERFORM FA-WRITE-DAP-LINE-INDEX-I                                
056400       END-IF                                                             
056500                                                                          
056600       PERFORM S50-SEND-CLOSE                                             
056700                                                                          
056800     ELSE                                                                 
056900                                                                          
057000       PERFORM S11-SKRIV-RUBRIKER                                         
057100       MOVE 1 TO I                                                        
057200       PERFORM UNTIL I > MAX-I                                            
057300         PERFORM S12-SKAPA-RAD                                            
057400         PERFORM S13-SKRIV-W61260-001-RAD                                 
057500         ADD 1 TO I                                                       
057600       END-PERFORM                                                        
057700                                                                          
057800     END-IF                                                               
057900     .                                                                    
058000                                                                          
058100     EJECT                                                                
058200 FA-WRITE-DAP-LINE-INDEX-I       SECTION.                                 
058300                                                                          
058400*    -- THE FOLLOWING FIELD IS NOT USED, BUT MUST REMAIN IN               
058500*    -- COPYTEXT FOR BACKWARD COMPABILITY WITH OLDER WEB                  
058600*    -- REPORTS. THE FIELD MUST ALSO BE INITIALIZED HERE                  
058700*    -- SINCE LOW-VALUE IS NOT ALLOWED IN WEB REPORTS.                    
058800     MOVE ZERO TO DAP-LINE-PTYP310-LINES                                  
058900                                                                          
059000     IF I = MAX-I                                                         
059100*      -- RECORD TYPE 2 FOR LINE WITH TOTALS                              
059200       MOVE '2         ' TO DAP-LINE-IDAFPRCD                             
059300     ELSE                                                                 
059400       MOVE '1         ' TO DAP-LINE-IDAFPRCD                             
059500     END-IF                                                               
059600                                                                          
059700     MOVE SPAR-IDDC        TO DAP-LINE-IDDC                               
059800     MOVE DAGENS-DATUM     TO DAP-LINE-TIAAMMDD                           
059900                                                                          
060000     EVALUATE I                                                           
060100      WHEN IA      MOVE 'A'   TO DAP-LINE-KDREFTYP                        
060200      WHEN IB      MOVE 'B'   TO DAP-LINE-KDREFTYP                        
060300      WHEN IT      MOVE 'T'   TO DAP-LINE-KDREFTYP                        
060400      WHEN IZ      MOVE 'Z'   TO DAP-LINE-KDREFTYP                        
060500      WHEN MAX-I   MOVE '9'   TO DAP-LINE-KDREFTYP                        
060600     END-EVALUATE                                                         
060700                                                                          
060800*    -- CONVERT TOTAL TIMES TO MEAN TIME                                  
060900     IF NOT BINNED-L(I) = 0                                               
061000       COMPUTE TIME-TOT(I) ROUNDED =                                      
061100          TIME-TOT(I) / BINNED-L(I)                                       
061200     END-IF                                                               
061300                                                                          
061400     IF NOT TIME-LINE-PRIO(I) = 0                                         
061500       COMPUTE TIMEPRIO(I) ROUNDED =                                      
061600          TIMEPRIO(I) / TIME-LINE-PRIO(I)                                 
061700     END-IF                                                               
061800                                                                          
061900*    IF DCS-FLPRISSPR = JA                                                
062000*       MOVE ZEROES TO BINNED-V(I)                                        
062100*    END-IF                                                               
062200                                                                          
062300     MOVE AK-LINES(I)              TO DAP-LINE-AK-LINES                   
062400     MOVE BINNED-L(I)              TO DAP-LINE-BINNED-LINES               
062500     MOVE TIME-LINE-PRIO(I)        TO DAP-LINE-BINNED-PRIO                
062600     MOVE TIME-TOT(I)              TO DAP-LINE-TIME-TOT                   
062700     MOVE TIMEPRIO(I)              TO DAP-LINE-DAYS-PRIO                  
062800*    MOVE BINNED-V(I)              TO DAP-LINE-BINNED-V                   
062900                                                                          
063000     PERFORM S50-PUT-REPORT-LINE                                          
063100     .                                                                    
063200                                                                          
063300     EJECT                                                                
063400 FB-WRITE-DAP-LINE-CDC       SECTION.                                     
063500                                                                          
063600     EVALUATE I                                                           
063700       WHEN IA                                                            
063800                                                                          
063900         MOVE 'AIR'              TO W002-KDREFTYP                         
064000       WHEN IB                                                            
064100         MOVE 'BOAT'             TO W002-KDREFTYP                         
064200       WHEN IT                                                            
064300         MOVE 'TRANSFERS'        TO W002-KDREFTYP                         
064400       WHEN IZ                                                            
064500         MOVE 'OTHERS'           TO W002-KDREFTYP                         
064600       WHEN MAX-I                                                         
064700         MOVE 'SUMMARY'          TO W002-KDREFTYP                         
064800     END-EVALUATE                                                         
064900                                                                          
065000*    -- CONVERT TOTAL TIMES TO MEAN TIME                                  
065100     IF NOT BINNED-L(I) = 0                                               
065200       COMPUTE TIME-TOT(I) ROUNDED =                                      
065300          TIME-TOT(I) / BINNED-L(I)                                       
065400     END-IF                                                               
065500                                                                          
065600     IF NOT TIME-LINE-PRIO(I) = 0                                         
065700       COMPUTE TIMEPRIO(I) ROUNDED =                                      
065800          TIMEPRIO(I) / TIME-LINE-PRIO(I)                                 
065900     END-IF                                                               
066000                                                                          
066100     MOVE AK-LINES(I)              TO W002-AK-LINES                       
066200     MOVE BINNED-L(I)              TO W002-BINNED-LINES                   
066300     MOVE TIME-LINE-PRIO(I)        TO W002-BINNED-PRIO                    
066400     MOVE TIME-TOT(I)              TO W002-TIME-TOT                       
066500     MOVE TIMEPRIO(I)              TO W002-DAYS-PRIO                      
066600                                                                          
066700     MOVE W002-DETAIL              TO W002-LINE                           
066800     PERFORM S50-PUT-W002-REPORT-LINE                                     
066900     .                                                                    
067000                                                                          
067100     EJECT                                                                
067200 Z-FINIT SECTION.                                                         
067300     CLOSE W61256                                                         
067400           W61260-001                                                     
067500     SKIP2                                                                
067600     MOVE 'S' TO POSTSUM-OPKOD                                            
067700     CALL POSTSUM USING POSTSUM-PARM                                      
067800     .                                                                    
067900     EJECT                                                                
068000 S01-LAES-W61256  SECTION.                                                
068100     READ W61256 INTO IN-AREA                                             
068200     AT END                                                               
068300        MOVE HIGH-VALUE TO IN-AREA                                        
068400        SET END-OF-W61256 TO TRUE                                         
068500                                                                          
068600     NOT AT END                                                           
068700        MOVE 'W61256' TO POSTSUM-FDNAMN                                   
068800        MOVE 'W61260D1' TO POSTSUM-DDNAMN2                                
068900        MOVE IN-SHIST-IDDC TO POSTSUM-TRANSTYP                            
069000        CALL POSTSUM USING POSTSUM-PARM                                   
069100     END-READ                                                             
069200                                                                          
069300     .                                                                    
069400     EJECT                                                                
069500 S11-SKRIV-RUBRIKER SECTION.                                              
069600                                                                          
069700     MOVE 1 TO W001-SIDRAKNARE                                            
069800     MOVE 3 TO W001-SKIP                                                  
069900     MOVE +9 TO W001-ANTAL-RADER                                          
070000     MOVE DAGENS-DATUM    TO W001-DATUM                                   
070100     MOVE W001-SIDRAKNARE TO W001-SID                                     
070200     MOVE SPAR-IDDC TO RUBRIK-DC                                          
070300                                                                          
070400     WRITE W61260-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
070500     MOVE 1 TO W001-SKIP                                                  
070600     WRITE W61260-001-RAD FROM W001-RUBRIK2 AFTER W001-SKIP               
070700     WRITE W61260-001-RAD FROM W001-RUBRIK3 AFTER W001-SKIP               
070800     MOVE 2 TO W001-SKIP                                                  
070900     MOVE SPACE TO W001-RAD                                               
071000     .                                                                    
071100                                                                          
071200     EJECT                                                                
071300 S11-WRITE-DAP-HEADER SECTION.                                            
071400                                                                          
071500     MOVE 1                          TO HDR-REQU-IDMSGVER                 
071600     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
071700     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
071800                                                                          
071900     MOVE SPACE                      TO HDR-IDOUTREC                      
072000                                                                          
072100     IF DCS-CDC                                                           
072200       MOVE SPACE                    TO HDR-IDOUTTYPE                     
072300       MOVE 'W61260-0'               TO HDR-IDOUTTYPE(1:8)                
072400       MOVE SPAR-IDDC                TO HDR-IDOUTTYPE(9:2)                
072500       MOVE 'W61260'                 TO HDR-IDOUTREC                      
072600     ELSE                                                                 
072700       MOVE 'REFILL-AK-DAY'          TO HDR-IDOUTTYPE                     
072800       MOVE SPAR-IDDC                TO HDR-IDOUTREC(1:2)                 
072900       MOVE 'W61260'                 TO HDR-IDOUTREC(3:8)                 
073000     END-IF                                                               
073100     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
073200                                                                          
073300     PERFORM S50-PUT-HEADER                                               
073400                                                                          
073500*    WRITE HEADERS FOR CDC                                                
073600     IF DCS-CDC                                                           
073700       MOVE SPAR-IDDC                TO W002-IDDC                         
073800       MOVE DAGENS-DATUM             TO W002-DATUM                        
073900                                                                          
074000       MOVE W002-RAD1                TO W002-LINE                         
074100       PERFORM S50-PUT-W002-REPORT-LINE                                   
074200       MOVE SPACES                   TO W002-LINE                         
074300       PERFORM S50-PUT-W002-REPORT-LINE                                   
074400       MOVE W002-RAD2                TO W002-LINE                         
074500       PERFORM S50-PUT-W002-REPORT-LINE                                   
074600       MOVE W002-RAD3                TO W002-LINE                         
074700       PERFORM S50-PUT-W002-REPORT-LINE                                   
074800     END-IF                                                               
074900     .                                                                    
075000                                                                          
075100     EJECT                                                                
075200 S12-SKAPA-RAD   SECTION.                                                 
075300                                                                          
075400     MOVE AK-LINES(I) TO TEMP-HELTAL                                      
075500     MOVE TEMP-HELTAL TO L-AK-LINES                                       
075600                                                                          
075700     MOVE AK-VALUE(I) TO TEMP-DECTAL                                      
075800     MOVE TEMP-DECTAL TO L-AK-VALUE                                       
075900                                                                          
076000     MOVE BO-VALUE(I) TO TEMP-DECTAL                                      
076100     MOVE TEMP-DECTAL TO L-BO-VALUE                                       
076200                                                                          
076300*    -- CONVERT TOTAL TIMES TO MEAN TIME                                  
076400     IF NOT TIME-TOT(I) = 0                                               
076500       COMPUTE TIME-TOT(I) ROUNDED =                                      
076600               TIME-TOT(I) / BINNED-L(I)                                  
076700     END-IF                                                               
076800                                                                          
076900     IF NOT TIMEPRIO(I) = 0                                               
077000       COMPUTE TIMEPRIO(I) ROUNDED =                                      
077100               TIMEPRIO(I) / TIME-LINE-PRIO(I)                            
077200     END-IF                                                               
077300                                                                          
077400     MOVE TIME-TOT(I)  TO TEMP-DECTAL2                                    
077500     MOVE TEMP-DECTAL2 TO L-TIME-TOT                                      
077600                                                                          
077700     MOVE TIMEPRIO(I)  TO TEMP-DECTAL2                                    
077800     MOVE TEMP-DECTAL2 TO L-TIMEPRIO                                      
077900                                                                          
078000     MOVE BINNED-L(I) TO TEMP-HELTAL                                      
078100     MOVE TEMP-HELTAL TO L-BINNED-L                                       
078200                                                                          
078300     MOVE TIME-LINE-PRIO(I) TO TEMP-HELTAL                                
078400     MOVE TEMP-HELTAL TO L-BINNED-P                                       
078500                                                                          
078600     MOVE BINNED-V(I) TO TEMP-DECTAL                                      
078700     MOVE TEMP-DECTAL TO L-BINNED-V                                       
078800     .                                                                    
078900                                                                          
079000     EJECT                                                                
079100 S13-SKRIV-W61260-001-RAD SECTION.                                        
079200                                                                          
079300     MOVE W001-DETALJ  TO  W001-RAD                                       
079400     WRITE W61260-001-RAD FROM W001-RAD AFTER W001-SKIP                   
079500     MOVE SPACE TO W001-RAD                                               
079600     ADD  W001-SKIP TO W001-ANTAL-RADER                                   
079700     .                                                                    
079800                                                                          
079900     EJECT                                                                
080000 S21-TIDSBERAK SECTION.                                                   
080100                                                                          
080200                                                                          
080300     MOVE IN-SHIST-IDDC      TO  TIME-IDDC                                
080400     MOVE IN-SHIST-TIINLMOT  TO  TIME-TIINLMOT                            
080500     MOVE IN-SHIST-TIINLMTI  TO  TIME-TIINLMTI                            
080600     MOVE IN-SHIST-TIINLINL  TO  TIME-TIINLINL                            
080700     MOVE IN-SHIST-TIINLITI  TO  TIME-TIINLITI                            
080800                                                                          
080900     CALL W612TIME USING TIME-W612TID WDB6-PCB                            
081000                                                                          
081100     IF TIME-KDSVAR = JA                                                  
081200       ADD TIME-KVDAGDEC TO TEMP-TOT-TIME                                 
081300       IF IN-SHIST-FLPRIO = JA                                            
081400         ADD TIME-KVDAGDEC TO TEMP-PRIO-TIME                              
081500         MOVE 1 TO TEMP-TIME-LINE-PRIO                                    
081600       END-IF                                                             
081700     ELSE                                                                 
081800       MOVE 'FELAKTIG RETURKOD FRÅN W612TIME' TO FELTEXT-STR              
081900       DISPLAY FELTEXT                                                    
082000       PERFORM S99-ABEND                                                  
082100     END-IF                                                               
082200     .                                                                    
082300     EJECT                                                                
082400 S31-ASSIGN-TALLYS SECTION.                                               
082500                                                                          
082600     MOVE IN-SHIST-IDDISTR TO DIST35-IDDISTR                              
082700     IF DIST35-NA-TRANSFER                                                
082800     OR DIST35-CN-TRANSFER                                                
082900     OR DIST35-PACIFIC-TRANSFER                                           
082910     OR DIST35-REFILL-INOM-JP                                             
083000*      -- TRANSFER                                                        
083100       MOVE IT TO I                                                       
083200     ELSE                                                                 
083300*      --AIR/FLYG. AUSTRALA/JAPAN USES 19                                 
083400       IF IN-SHIST-KDFRAKT = 17 OR 19                                     
083500         MOVE IA TO I                                                     
083600       ELSE                                                               
083700*        --BOAT                                                           
083800         IF IN-SHIST-KDFRAKT >= 41 AND <= 45                              
083900           MOVE IB TO I                                                   
084000         ELSE                                                             
084100*          -- THEN IT IS SOMETHING ELSE                                   
084200           MOVE IZ TO I                                                   
084300         END-IF                                                           
084400       END-IF                                                             
084500     END-IF                                                               
084600                                                                          
084700*    -- 310 DATA - CURRENT TYPE AND TOTALS                                
084800     ADD  TEMP-LINE-AK   TO AK-LINES(I)                                   
084900     ADD  TEMP-VALUE-AK  TO AK-VALUE(I)                                   
085000     ADD  TEMP-VALUE-BO  TO BO-VALUE(I)                                   
085100                                                                          
085200     ADD  TEMP-LINE-AK   TO AK-LINES(MAX-I)                               
085300     ADD  TEMP-VALUE-AK  TO AK-VALUE(MAX-I)                               
085400     ADD  TEMP-VALUE-BO  TO BO-VALUE(MAX-I)                               
085500                                                                          
085600*    -- R32 DATA - CURRENT TYPE AND TOTALS                                
085700     ADD  TEMP-TOT-TIME  TO TIME-TOT(I)                                   
085800     ADD  TEMP-PRIO-TIME TO TIMEPRIO(I)                                   
085900     ADD  TEMP-LINE-BI   TO BINNED-L(I)                                   
086000     ADD  TEMP-TIME-LINE-PRIO TO TIME-LINE-PRIO(I)                        
086100     ADD  TEMP-VALUE-BI  TO BINNED-V(I)                                   
086200                                                                          
086300     ADD  TEMP-TOT-TIME  TO TIME-TOT(MAX-I)                               
086400     ADD  TEMP-PRIO-TIME TO TIMEPRIO(MAX-I)                               
086500     ADD  TEMP-LINE-BI   TO BINNED-L(MAX-I)                               
086600     ADD  TEMP-TIME-LINE-PRIO TO TIME-LINE-PRIO(MAX-I)                    
086700     ADD  TEMP-VALUE-BI  TO BINNED-V(MAX-I)                               
086800                                                                          
086900     .                                                                    
087000     EJECT                                                                
087100 S41-NOLLSTAELL-TEMP-FAELT SECTION.                                       
087200                                                                          
087300     MOVE ZERO TO TEMP-LINE-AK                                            
087400     MOVE ZERO TO TEMP-LINE-BI                                            
087500     MOVE ZERO TO TEMP-VALUE-AK                                           
087600                                                                          
087700     MOVE ZERO TO TEMP-VALUE-BO                                           
087800     MOVE ZERO TO TEMP-VALUE-BI                                           
087900     MOVE ZERO TO TEMP-TOT-TIME                                           
088000     MOVE ZERO TO TEMP-PRIO-TIME                                          
088100     MOVE ZERO TO TEMP-TIME-LINE-PRIO                                     
088200     .                                                                    
088300     EJECT                                                                
088400 S50-SEND-OPEN SECTION.                                                   
088500                                                                          
088600     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
088700     MOVE 'OPEN'                          TO SEND-KDFUNC                  
088800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
088900                         SEND-OPEN-AREA                                   
089000     IF SEND-KDRC > ZERO                                                  
089100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
089200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
089300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
089400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
089500     END-IF                                                               
089600     .                                                                    
089700     SKIP3                                                                
089800 S50-PUT-HEADER SECTION.                                                  
089900                                                                          
090000     MOVE 'PUT'                           TO SEND-KDFUNC                  
090100     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
090200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
090300                         SEND-KVDLEN                                      
090400                         HDR-AREA                                         
090500     IF SEND-KDRC > ZERO                                                  
090600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
090700       STRING 'WZ01SEND PUT HEAD  RC=' KDRC-DISPLAY                       
090800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
090900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300 S50-PUT-REPORT-LINE    SECTION.                                          
091400                                                                          
091500     MOVE 'PUT'                           TO SEND-KDFUNC                  
091600     MOVE LENGTH OF DAP-LINE-AREA         TO SEND-KVDLEN                  
091700*    -- TEMP FIX                                                          
091800*    DISPLAY DAP-LINE-AREA(1:SEND-KVDLEN)                                 
091900                                                                          
092000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
092100                         SEND-KVDLEN                                      
092200                         DAP-LINE-AREA                                    
092300     IF SEND-KDRC > ZERO                                                  
092400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
092500       STRING 'WZ01SEND PUT LINE  RC=' KDRC-DISPLAY                       
092600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
092700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
092800     END-IF                                                               
092900     .                                                                    
093000     SKIP3                                                                
093100 S50-PUT-W002-REPORT-LINE   SECTION.                                      
093200                                                                          
093300     MOVE 'PUT'                           TO SEND-KDFUNC                  
093400     MOVE LENGTH OF W002-LINE             TO SEND-KVDLEN                  
093500                                                                          
093600*    DISPLAY W002-LINE(1:SEND-KVDLEN)                                     
093700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
093800                         SEND-KVDLEN                                      
093900                         W002-LINE                                        
094000     IF SEND-KDRC > ZERO                                                  
094100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
094200       STRING 'WZ01SEND PUT LINE  RC=' KDRC-DISPLAY                       
094300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
094400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
094500     END-IF                                                               
094600     .                                                                    
094700     SKIP3                                                                
094800 S50-SEND-CLOSE SECTION.                                                  
094900                                                                          
095000     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
095100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
095200     .                                                                    
095300 S99-ABEND SECTION.                                                       
095400                                                                          
095500     SKIP2                                                                
095600     MOVE 'S' TO POSTSUM-OPKOD                                            
095700     CALL POSTSUM USING POSTSUM-PARM                                      
095800     CALL ABEND USING RKOD-ABEND                                          
095900     .                                                                    
096000     EJECT                                                                
096100* --- IMS SEKTIONER ---                                                   
096200     SKIP3                                                                
096300                                                                          
096400 IMS-GU-WDB601    SECTION.                                                
096500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
096600          DELIMITED BY SIZE INTO SSA1                                     
096700     MOVE '  GE' TO GODK-STATUSKODER                                      
096800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
096900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     IF SEGMENT-SAKNAS                                                    
097200        MOVE SPACE TO DCS-KDDC                                            
097300     END-IF                                                               
097400     .                                                                    
097500     EJECT                                                                
097600 IMS-STATUSKONTROLL SECTION.                                              
097700                                                                          
097800     SET STATUS-IX TO 1                                                   
097900     SEARCH GODK-STATUS                                                   
098000       AT END                                                             
098100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
098200           DELIMITED BY SIZE INTO FELTEXT                                 
098300         DISPLAY FELTEXT                                                  
098400         CALL FELLOG                                                      
098500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
098600         CONTINUE                                                         
098700     END-SEARCH                                                           
098800     .                                                                    
