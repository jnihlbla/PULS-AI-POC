001037*COMPOPT VAMODE24=YES                                                     
001038 PROCESS DATA(24)                                                         
001039**COMPOPT AMODE=ANY INCLMOD=VA24UOPT                                      
010000 ID DIVISION.                                                             
020000 PROGRAM-ID.     W0920600.                                                
030000*AUTHOR.         TOMAS SVENSSON.                                          
040000*DATE-WRITTEN.   FEB 1976.                                                
050000*REMARKS.                                                                 
060000*    FUNKTION                                                             
070006*        "IN-TRATTEN"                                                     
080006*        PROGRAMMET ÄR EN STYR-MODUL FÖR KONTROLLER AV HÅLKORTS-          
090006*        TRANSAKTIONER SOM STANSATS PÅ STANSBYRÅ ELLER VIA MEMO           
100006*        ELLER GENERERATS PÅ ANNAT SÄTT VIA NÅGOT SYSTEM.                 
110006*                                                                         
120006*        INLÄSTA TRANSAKTIONER SORTERAS I TRANSAKTIONSTYPS-ORDNING        
130006*        OCH KONTROLLERAS FORMELLT VIA EN REGEL-MODUL SOM SKAPATS         
140006*        UTIFRÅN "TRATT-REGLER" I DATACPY-BIBLIOTEKET. EN REGEL-          
150006*        MODUL FÖR TRANSAKTIONSTYP "XXX" HETER "W092RXXX".                
160006*                                                                         
170007*        FORMELLT RIKTIGA TRANSAKTIONER SKICKAS FÖR VIDARE                
180006*        BEHANDLING TILL OLIKA SUBMODULER. VILKEN SUBMODUL SOM            
190006*        HAR HAND OM VILKA TRANSAKTIONER STYRS AV EN TABELL               
200006*        SOM LÄSES IN FRÅN EN CONSTANT-MEDLEM.                            
210006*        SUBMODULEN GÖR EVENTUELLT YTTERLIGARE KONTROLLER OCH             
220006*        SKRIVER SEN UT TRANSAKTIONEN PÅ RÄTT TRANSAKTIONS-FIL.           
230006*                                                                         
240006*        SAMTLIGA TRANSAKTIONER IN SKRIVS PÅ EN KONTROLL-LISTA.           
250006*        DE SOM ÄR FELAKTIGA SKRIVS PÅ EN FEL-FIL SOM SKICKAS             
260006*        TILL "UT-TRATTEN" FÖR UTSKRIFT.                                  
270006*                                                                         
280000*    INDATA                                                               
290006*        (1) TABELL FRÅN CONSTANT-BIBLIOTEKET SOM ANGER ALLA              
300006*            TRANSAKTIONS-TYPER OCH VILKEN SUBMODUL SOM SKÖTER            
310006*            UTSKRIFT (OCH VISSA KONTROLLER).                             
320006*        (2) ETT ANTAL KONKATENERADE FILER MED HÅLKORTS-                  
330006*            TRANSAKTIONER SOM FÖRST SORTERAS PÅ MODULNAMN (ENLIGT        
340006*            CONSTANT-MEDLEMMEN) OCH KORTTYP INNAN DE BEHANDLAS.          
350006*        (3) GLURPKODS-REGLER FÖR STYRNING AV BEHANDLIGEN AV              
360006*            R05:OR                                                       
370006*                                                                         
380000*    UTDATA.                                                              
400006*        FILE W09227, FELFIL TILL UT-TRATTEN                              
410006*        FILE W09226, AVSTÄMNINGSLISTA MED SUMMOR/TRANS                   
420006*        DIVERSE FILER SOM ÖPPNAS OCH STÄNGS I SUBMODULERNA.              
430006*                                                                         
440000*    SUBPROGRAM                                                           
450006*        REGEL-MODUL W092RXXX BEROENDE PÅ TRANSTYP XXX.                   
460006*        OLIKA BEHANDLINGS-MODULER ENLIGT CONSTANT-TABELL.                
470000*        GENERELLA SUBPROGRAM:                                            
480006*        DATKORT, ABEND, OPNCLOSE.                                        
490006*                                                                         
500000*    RETURKODER                                                           
510000*        +0  NORMALT SLUT.                                                
520006*        AVBROTT U0020     FEL FRÅN SORT.                                 
530006*                                                                         
540000     EJECT                                                                
550000 ENVIRONMENT DIVISION.                                                    
560000 INPUT-OUTPUT SECTION.                                                    
570000 FILE-CONTROL.                                                            
580012                                                                          
590012*** OBS: ANLEDNINGEN TILL ATT UDDA DD-NAMN ANVÄNDS ÄR ATT DESSA           
600012*** FILER INTE SKA ÖPPNAS OCH STÄNGAS VID ANROP AV OPNCLOSE,              
610012*** UTAN ENDAST DE SOM ANVÄNDS I SUBMODULERNA.                            
620012                                                                          
630006     SELECT  INFIL    ASSIGN TO INTRANS.                                  
650012     SELECT  W09227   ASSIGN TO FELDD.                                    
660021**   SELECT  W09226   ASSIGN TO SYSOUT. (DISPLAY)                         
670006     SELECT  EXTTAB   ASSIGN TO TABELL.                                   
680012     SELECT  SORTFIL  ASSIGN TO W09206DS.                                 
690000     EJECT                                                                
700000 DATA DIVISION.                                                           
710000 FILE SECTION.                                                            
720000     SKIP2                                                                
730000 FD  INFIL                                                                
740000     RECORDING V                                                          
750000     BLOCK CONTAINS 0 RECORDS.                                            
760022                                                                          
770000 01  INPOST              PIC X(100).                                      
780000     SKIP2                                                                
850006 FD  W09227                                                               
860000     RECORDING V                                                          
870000     BLOCK CONTAINS 0 RECORDS.                                            
880022                                                                          
890000 01  FEL-POST            PIC X(116).                                      
900000     SKIP2                                                                
900122 FD  EXTTAB                                                               
900222     RECORDING F                                                          
900322     BLOCK CONTAINS 0 RECORDS.                                            
900422                                                                          
901022 01  FILLER                  PIC X(80).                                   
960022     SKIP2                                                                
010007 SD  SORTFIL.                                                             
011022                                                                          
020000 01  SD-SORT-POST.                                                        
030000     03  SD-MODUL            PIC X(6).                                    
040000     03  SD-KORT.                                                         
050000         05  SD-IDKTYP       PIC X(3).                                    
060000         05  FILLER          PIC X(157).                                  
070000     EJECT                                                                
080000 WORKING-STORAGE SECTION.                                                 
090000     SKIP2                                                                
090130                                                                          
091030*    -- CHECKED BY WY2000                                                 
100007 77  JA                      PIC X                  VALUE 'J'.            
110007 77  NEJ                     PIC X                  VALUE 'N'.            
120007                                                                          
130007 77  TAB-INDX                PIC S9(9)  COMP SYNC.                        
140007 77  FELTAB-INDEX            PIC S9(9)  COMP SYNC.                        
150007 77  ANTAL-I-TAB             PIC S9(3)  COMP-3      VALUE +0.             
160007                                                                          
180007 77  TRANS-RIKTIG            PIC X                  VALUE 'J'.            
190013                                                                          
200013 77  NY-TRANSTYP             PIC X                  VALUE 'N'.            
210013 77  SPARAD-TRANSTYP         PIC X(3)               VALUE SPACE.          
220015                                                                          
230013 77  RIKTIGA-DENNA-TRANSTYP  PIC S9(9)  COMP-3      VALUE ZERO.           
240013 77  FELAKTIGA-DENNA-TRANSTYP PIC S9(9)  COMP-3     VALUE ZERO.           
250015                                                                          
260013 77  TOT-RIKTIGA             PIC S9(9)  COMP-3      VALUE ZERO.           
270013 77  TOT-FELAKTIGA           PIC S9(9)  COMP-3      VALUE ZERO.           
280015                                                                          
290016 77  DIS-RIKTIGA             PIC Z(8)9.                                   
300016 77  DIS-FELAKTIGA           PIC Z(8)9.                                   
310016 77  DIS-TOTALT              PIC Z(8)9.                                   
320007                                                                          
330015                                                                          
340007 77  EOF-INFIL               PIC X                  VALUE 'N'.            
350007 77  EOF-EXTTAB              PIC X                  VALUE 'N'.            
360007 77  EOF-SORTFIL             PIC X                  VALUE 'N'.            
370007                                                                          
380007 77  WRETKOD                 PIC S9(4) COMP SYNC    VALUE +0.             
390007 77  RKOD                    PIC S9(4) COMP SYNC    VALUE +0.             
400010     EJECT                                                                
410010 01  FILLER              PIC X(20)   VALUE ALL 'W'.                       
420000     SKIP2                                                                
430000 01  TABELL.                                                              
440000     03  KTYP-TAB OCCURS 150                                              
450000         INDEXED BY KTYP-INDX.                                            
460000         05  TAB-IDKTYP       PIC X(3).                                   
470000         05  TAB-MODUL        PIC X(6).                                   
480000     SKIP2                                                                
490000 01  W-EXTTAB.                                                            
500000     03  W-EXTTAB-IDKTYP     PIC X(3).                                    
510000     03  FILLER              PIC X.                                       
520000     03  W-EXTTAB-MODUL      PIC X(6).                                    
530000     03  FILLER              PIC X(70).                                   
540000     EJECT                                                                
550010 01  FILLER              PIC X(20)   VALUE ALL 'A'.                       
560000***  INLÄSNINGS-AREA FÖR W09230.                               ***        
570000     SKIP2                                                                
580000 01  IN-TRANS.                                                            
590000     03  IN-MODULNAMN        PIC X(6).                                    
600000     03  IN-KORT.                                                         
610000         05  IN-IDKTYP       PIC X(3).                                    
620000         05  FILLER          PIC X(157).                                  
630007     03  FILLER REDEFINES IN-KORT.                                        
640007*        05  -COPY W222R89T -PRE R89-                                     
650000     EJECT                                                                
660000 01  W-IN-TRANS.                                                          
670007     03  W-IN-IDKTYP     PIC X(3).                                        
680007     03  FILLER          PIC X(77).                                       
690007     03  FILLER          PIC X(80).                                       
700000     SKIP2                                                                
710011 01  URSPR-R89-IDPTYP        PIC X(3).                                    
720000     SKIP2                                                                
730010 01  DENNA-TRANS             PIC X(3).                                    
740010     SKIP2                                                                
750011 01  R89-UTAREA              PIC X(100).                                  
760000     EJECT                                                                
770000 01  FILLER              PIC X(20)   VALUE ALL 'B'.                       
780007***  -- AREOR FÖR ANROP AV REGEL-MODULERNA --                             
790000     SKIP2                                                                
800000 01  V165-TRANS.                                                          
810000     03  V165-IDDEL         PIC X(36).                                    
820007     03  FILLER REDEFINES V165-IDDEL.                                     
830007       05  -COPY W092W001 -PRE V165-                                      
840000     03  V165-UTPOST        PIC X(214).                                   
850000     SKIP2                                                                
860000 01  V165-FELAREA.                                                        
870000     03  V165-FELKOD        PIC X(3).                                     
880000     03  V165-FELTEXT       PIC X(30).                                    
890000     03  V165-FELIND        PIC X(100).                                   
900000     SKIP2                                                                
910000 01  V165-RETURKOD          PIC X.                                        
920000     88  V165-RETURKOD-OK       VALUE SPACE.                              
930000     88  V165-RETURKOD-FEL      VALUE 'E'.                                
940000     SKIP2                                                                
950000 01  REGEL-MODULNAMN.                                                     
960007     03  FILLER             PIC X(5)   VALUE 'W092R'.                     
970007     03  V165-TRANSTYP      PIC X(3)   VALUE SPACE.                       
980000 01  REGELMODUL REDEFINES REGEL-MODULNAMN   PIC X(8).                     
990000     EJECT                                                                
000000 01  FILLER                  PIC X(20)   VALUE ALL 'C'.                   
010000     SKIP2                                                                
020000 01  TRANS-PARM.                                                          
030000     03  TRANS-KOD           PIC X(3).                                    
040000     03  TRANS-FELTAB    OCCURS 100.                                      
050000         05  TRANS-FELKOD    PIC X(3).                                    
060000         05  TRANS-FELTEXT   PIC X(15).                                   
070000     03  FILLER              PIC X(3)    VALUE HIGH-VALUE.                
080000     03  TRANS-KORT          PIC X(100).                                  
090000     03  TRANS-UTAREA.                                                    
100000*        05  IDDEL   -COPY W092W001  -PRE TRANS-.                         
110000         05  TRANS-UTPOST    PIC X(214).                                  
120000     EJECT                                                                
130000 01  FILLER              PIC X(20)   VALUE ALL 'D'.                       
140000 01  W-FEL-POST.                                                          
150000*    03  FEL-IDDEL   -COPY W092W001  -PRE W-.                             
160000     03  W-FELTEXT           PIC X(80).                                   
170000     EJECT                                                                
180000 01  FILLER              PIC X(20)   VALUE ALL 'E'.                       
190000 01  W-KONTROLL-AREA.                                                     
200000*    03  W-KONTROLL-IDDEL -COPY W092W001.                                 
210000     03  W-KONTROLL-POST     PIC X(100).                                  
220000     03  FILLER REDEFINES W-KONTROLL-POST.                                
230000         05  FILLER          PIC X(12).                                   
240000         05  R05-GLURPKOD    PIC X(16).                                   
250000         05  FILLER          PIC X(52).                                   
260000     03  FILLER REDEFINES W-KONTROLL-POST.                                
270000         05  FILLER          PIC X(12).                                   
280000         05  R22-IDBESTNR    PIC X(10).                                   
290000         05  FILLER          PIC X(57).                                   
300027     EJECT                                                                
310000 01  DATUM-PARM.                                                          
320000     03  KALLANDE-PGM        PIC X(6)    VALUE 'W09206'.                  
330000     03  SOEK-ID             PIC X(6)    VALUE 'WDATUM'.                  
340007*    03  DATUM   -COPY WDATKORT                                           
350027     EJECT                                                                
360000 01  W-DATUM.                                                             
370000     03  W-AAR               PIC 99.                                      
380000     03  FILLER              PIC X   VALUE '.'.                           
390000     03  W-MAAN              PIC 99.                                      
400000     03  FILLER              PIC X   VALUE '.'.                           
410000     03  W-DAG               PIC 99.                                      
420000     SKIP2                                                                
430000 01  PARMVAERDE.                                                          
440000     03  LAENGD              PIC S9(4)   COMP SYNC.                       
450000     03  PARAMETER           PIC X(8).                                    
460007                                                                          
470000 01  GEN-SUB-PROGRAM.                                                     
480000     03  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
490000     03  OPNCLOSE            PIC X(8)    VALUE 'OPNCLOSE'.                
500007     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
510007 01  OVR-SUB-PROGRAM.                                                     
530000     03  W09289              PIC X(8)    VALUE 'W09289  '.                
540007     03  INLADDAD-MODUL      PIC X(8)    VALUE '        '.                
560000     EJECT                                                                
960000 PROCEDURE DIVISION.                                                      
970007     SKIP2                                                                
980000     PERFORM A-HK                                                         
990000     SORT SORTFIL                                                         
000007      ASCENDING SD-MODUL SD-IDKTYP                                        
010007      INPUT PROCEDURE B-LAMNA-TILL-SORT                                   
020007      OUTPUT PROCEDURE C-HAMTA-FRAN-SORT                                  
030000     SKIP2                                                                
040000     IF SORT-RETURN > 0                                                   
050000       DISPLAY ' SORT RETURN EJ = 0'                                      
060000       MOVE +20 TO RKOD                                                   
070007       CALL ABEND USING RKOD                                              
080000     ELSE                                                                 
090000       PERFORM D-FINALEN                                                  
100026       MOVE RKOD TO RETURN-CODE                                           
110000       GOBACK                                                             
120000     END-IF                                                               
130000     CONTINUE.                                                            
140000     EJECT                                                                
150000 A-HK SECTION.                                                            
160000     SKIP2                                                                
170000******************************************************************        
180000*    SAMTLIGA FILER MED DDNAMN SOM BÖRJAR PÅ R I STEG W09206     *        
190000*    ÖPPNAS GENOM ANROP PÅ OPNCLOSE (V16266). DATUMKORT ANROPAS. *        
200007*    FILERNA ÖPPNAS OCH LISTAN INITIERAS.                        *        
210000******************************************************************        
220007                                                                          
230000     MOVE +1 TO LAENGD                                                    
240028     MOVE 'W' TO PARAMETER                                                
250000     CALL OPNCLOSE USING PARMVAERDE                                       
260007                                                                          
270000     CALL DATKORT USING KALLANDE-PGM, SOEK-ID, DATUMKORT                  
280000     MOVE D-AAR TO W-AAR                                                  
290000     MOVE D-MAANAD TO W-MAAN                                              
300000     MOVE D-DAG TO W-DAG                                                  
310007                                                                          
320000     OPEN INPUT  INFIL                                                    
330035         OUTPUT  W09227                                                   
350007                                                                          
360000     OPEN INPUT EXTTAB                                                    
370007                                                                          
380000     PERFORM S70-LAES-EXTTAB                                              
390000     MOVE +1 TO TAB-INDX                                                  
400007     PERFORM UNTIL EOF-EXTTAB = JA                                        
410000       MOVE W-EXTTAB-IDKTYP TO TAB-IDKTYP (TAB-INDX)                      
420007       MOVE W-EXTTAB-MODUL  TO TAB-MODUL (TAB-INDX)                       
430000       ADD +1 TO TAB-INDX                                                 
440000       PERFORM S70-LAES-EXTTAB                                            
450000     END-PERFORM                                                          
460000     MOVE TAB-INDX TO ANTAL-I-TAB                                         
470007                                                                          
480000     CLOSE EXTTAB                                                         
490007                                                                          
500018     PERFORM S80-DISP-W09206-001-RUBRIKER                                 
510007     .                                                                    
520000     EJECT                                                                
530000 B-LAMNA-TILL-SORT SECTION.                                               
540000     SKIP2                                                                
550000     PERFORM S10-LAES-INFIL                                               
560007     PERFORM UNTIL EOF-INFIL = JA                                         
570007                                                                          
580011       IF W-IN-IDKTYP =  'R89'                                            
590011*        -- FRÅN VARJE R89 GENERERAS ETT ANTAL                            
600007*        -- TRANSAR BEROENDE PÅ INNEHÅLLET.                               
610011*        -- SJÄLVA R89:AN SKICKAS VIDARE FÖR ATT KOMMA                    
620007*        -- MED PÅ AVSTÄMNINGS- OCH KONTROLL-LISTAN                       
630011         MOVE W-IN-IDKTYP TO URSPR-R89-IDPTYP                             
640011         PERFORM UNTIL URSPR-R89-IDPTYP = SPACE                           
650011           MOVE SPACE TO R89-UTAREA                                       
660011           CALL W09289 USING W-IN-TRANS URSPR-R89-IDPTYP                  
670011                             R89-UTAREA                                   
680011           IF URSPR-R89-IDPTYP NOT = SPACE                                
690011             MOVE URSPR-R89-IDPTYP TO DENNA-TRANS                         
700007             PERFORM BA-SOEK-MODUL-FOER-DENNA-TRANS                       
710011             MOVE R89-UTAREA TO SD-KORT                                   
720007             RELEASE SD-SORT-POST                                         
730011             MOVE 'R89' TO URSPR-R89-IDPTYP                               
740007           END-IF                                                         
750007         END-PERFORM                                                      
760007       END-IF                                                             
770007                                                                          
780010       MOVE W-IN-IDKTYP TO DENNA-TRANS                                    
790007       PERFORM BA-SOEK-MODUL-FOER-DENNA-TRANS                             
800007       MOVE W-IN-TRANS TO SD-KORT                                         
810007                                                                          
820000       RELEASE SD-SORT-POST                                               
830000       PERFORM S10-LAES-INFIL                                             
840000     END-PERFORM                                                          
850007     .                                                                    
860000     EJECT                                                                
870007  BA-SOEK-MODUL-FOER-DENNA-TRANS SECTION.                                 
880007                                                                          
890007     SET KTYP-INDX TO 1                                                   
900007     SEARCH KTYP-TAB VARYING KTYP-INDX                                    
910010     WHEN DENNA-TRANS = TAB-IDKTYP (KTYP-INDX)                            
920007       MOVE TAB-MODUL (KTYP-INDX) TO SD-MODUL                             
930007     WHEN KTYP-INDX > ANTAL-I-TAB                                         
940007       MOVE '000000' TO SD-MODUL                                          
950007     END-SEARCH                                                           
960007     .                                                                    
970007     EJECT                                                                
980007  C-HAMTA-FRAN-SORT SECTION.                                              
990007                                                                          
000000     PERFORM S15-RETURN-SORTFIL                                           
010007                                                                          
020007     PERFORM UNTIL EOF-SORTFIL = JA OR IN-MODULNAMN = '000000'            
030007                                                                          
040011       IF IN-IDKTYP =  'R89'                                              
050013         PERFORM CC-BEH-KORREKT-R89                                       
060000       ELSE                                                               
070000         PERFORM CA-TEST-MOT-REGELMODUL                                   
080000         IF TRANS-RIKTIG = JA                                             
090000           PERFORM CB-BEH-KORREKT-TRANS                                   
100000         END-IF                                                           
110013       END-IF                                                             
120021                                                                          
130017       IF NY-TRANSTYP = JA                                                
140018         PERFORM S81-DISP-W09206-001-TRANSSUM                             
150017       END-IF                                                             
160021                                                                          
170013       IF TRANS-RIKTIG = JA                                               
180013         ADD 1 TO RIKTIGA-DENNA-TRANSTYP                                  
190013       ELSE                                                               
200013         ADD 1 TO FELAKTIGA-DENNA-TRANSTYP                                
210013       END-IF                                                             
220021                                                                          
240000       PERFORM S15-RETURN-SORTFIL                                         
250007                                                                          
260000     END-PERFORM                                                          
270013                                                                          
280021     MOVE V165-TRANSTYP TO SPARAD-TRANSTYP                                
290018     PERFORM S81-DISP-W09206-001-TRANSSUM                                 
300018     PERFORM S82-DISP-W09206-001-TOTSUM                                   
310007                                                                          
320000     IF INLADDAD-MODUL NOT = SPACE                                        
330000       MOVE 'EOF' TO TRANS-KOD                                            
340000       PERFORM S60-CALL-TRANSMODUL                                        
350000     END-IF                                                               
360007                                                                          
361021                                                                          
370000     IF IN-MODULNAMN = '000000'                                           
380007*      -- TRANSAR SOM SAKNAR BEHANDLINGS-MODUL --                         
381026       MOVE +8 TO RKOD                                                    
390021       PERFORM S90-DISP-W09206-002-RUBRIKER                               
391021       MOVE IN-IDKTYP TO SPARAD-TRANSTYP                                  
400021                                                                          
410007       PERFORM UNTIL EOF-SORTFIL = JA                                     
411021                                                                          
420000         MOVE ZERO TO W-KONTROLL-IDDEL                                    
430000         MOVE IN-KORT TO W-KONTROLL-POST                                  
441021                                                                          
450021         IF IN-IDKTYP NOT = SPARAD-TRANSTYP                               
460021           PERFORM S92-DISP-W09206-002-TRANSSUM                           
461023           MOVE IN-IDKTYP TO SPARAD-TRANSTYP                              
470021         END-IF                                                           
480021         PERFORM S91-DISP-W09206-002-DETRAD                               
481021                                                                          
490000         PERFORM S15-RETURN-SORTFIL                                       
500000       END-PERFORM                                                        
510007                                                                          
511021       PERFORM S92-DISP-W09206-002-TRANSSUM                               
520021       PERFORM S93-DISP-W09206-002-TOTSUM                                 
530000     END-IF                                                               
540007     .                                                                    
550007     EJECT                                                                
560000 CA-TEST-MOT-REGELMODUL SECTION.                                          
570000     SKIP2                                                                
580000******************************************************************        
590007*    I DENNA SECTION SKICKAS EN KORT-TRANSAKTION TILL SIN        *        
600007*    REGEL-MODUL FÖR FORMELL KONTROLL.                           *        
610007*    VID BRYTNING PÅ TRANSAKTIONSSTYP GÖRS CANCEL PÅ FÖREGÅENDE  *        
620007*    MODUL OCH EN NY LADDAS IN.                                  *        
630007*                                                                *        
640007*    FLERA FEL KAN FÖREKOMMA I EN TRANS, OCH REGEL-MODULEN       *        
650007*    ANROPAS DÅ FLERA GÅNGER TILLS INGA FLER FEL ÅTERSTÅR.       *        
660007*    FÖR VARJE FEL SKRIVS EN POST PÅ FEL-FILEN, MEDAN KORREKTA   *        
670007*    TRANSAR PASSERAR VIDARE.                                    *        
680000******************************************************************        
690007                                                                          
700013     MOVE NEJ TO NY-TRANSTYP                                              
710007     IF V165-TRANSTYP NOT = IN-IDKTYP                                     
720017       IF V165-TRANSTYP NOT = SPACE                                       
730013         MOVE JA TO NY-TRANSTYP                                           
740013         MOVE V165-TRANSTYP TO SPARAD-TRANSTYP                            
750017         IF SPARAD-TRANSTYP NOT = 'R89'                                   
760017           CANCEL REGELMODUL                                              
770018         END-IF                                                           
780007       END-IF                                                             
790007       MOVE IN-IDKTYP TO V165-TRANSTYP                                    
800007     END-IF                                                               
810007                                                                          
820000     MOVE SPACE TO V165-TRANS, V165-FELAREA                               
830000     PERFORM S20-CALL-REGELMODUL                                          
840000     PERFORM S25-KOLLA-IDDEL                                              
850007                                                                          
860000     MOVE V165-IDDEL TO W-KONTROLL-IDDEL                                  
870000     MOVE +0 TO KDFELMRK                                                  
880000     MOVE '000' TO IDFELKODX                                              
890000     MOVE IN-KORT TO W-KONTROLL-POST                                      
900007                                                                          
910000     IF V165-RETURKOD-OK                                                  
920000       MOVE JA TO TRANS-RIKTIG                                            
930000     ELSE                                                                 
940000       MOVE NEJ TO TRANS-RIKTIG                                           
950007       PERFORM UNTIL NOT V165-RETURKOD-FEL                                
960000         MOVE V165-IDDEL TO W-FEL-IDDEL                                   
970000         MOVE V165-FELKOD TO W-IDFELKODX                                  
980000         MOVE +0 TO W-KDFELMRK                                            
990000         MOVE SPACE TO W-FELTEXT                                          
000000         PERFORM S30-SKRIV-FELFIL                                         
010000         MOVE SPACE TO V165-FELAREA                                       
020000         PERFORM S20-CALL-REGELMODUL                                      
030000       END-PERFORM                                                        
040000     END-IF                                                               
050007     .                                                                    
060000     EJECT                                                                
070000 CB-BEH-KORREKT-TRANS SECTION.                                            
080000     SKIP2                                                                
090000******************************************************************        
100000*    I DENNA SECTION SKICKAS TRANSAKTIONER TILL RESPEKTIVE       *        
110000*    TRANSMODUL. ENDAST DE TRANSAKTIONER SOM BEFANNS KORREKTA    *        
120007*    BERÖRS. TRANSMODULERNA LADDAS IN DYNAMISKT.                 *        
130000*    VID BRYTNING PÅ MODULNAMN GÖRS CANCEL PÅ INLÄST MODUL OCH   *        
140000*    EN NY TRANSMODUL LADDAS IN. VISSA TRANSMODULER UTFÖR KOPP-  *        
150000*    LADE KONTROLLER. DÄRFÖR TESTAS FELTABELLEN VID ÅTERKOMST.   *        
160007*                                                                *        
170007*    FELAKTIGA TRANSAKTIONER SKRIVS PÅ FEL-FILEN.                *        
180007*    KORREKTA TRANSAR SKRIVS UT AV TRANSMODULEN PÅ LÄMPLIG       *        
190007*    TRANSAKTIONSFIL.                                            *        
200000******************************************************************        
210007                                                                          
220007     IF IN-MODULNAMN NOT = INLADDAD-MODUL                                 
230007       IF INLADDAD-MODUL NOT = SPACE                                      
240007*        -- AVSLUTANDE STÄNGNINGS-ANROP TILL FÖREGÅENDE MODUL             
250000         MOVE 'EOF' TO TRANS-KOD                                          
260000         PERFORM S60-CALL-TRANSMODUL                                      
270000         CANCEL INLADDAD-MODUL                                            
280007       END-IF                                                             
290007       MOVE IN-MODULNAMN TO INLADDAD-MODUL                                
300007       MOVE SPACE TO TRANS-KOD                                            
310000     END-IF                                                               
320007                                                                          
330007                                                                          
340000     MOVE IN-KORT TO TRANS-KORT                                           
350000     MOVE V165-TRANS TO TRANS-UTAREA                                      
360000     MOVE +0 TO TRANS-KDFELMRK                                            
370000     MOVE '000' TO TRANS-IDFELKODX                                        
380000     PERFORM S60-CALL-TRANSMODUL                                          
390007                                                                          
400000     IF TRANS-FELKOD (1) = HIGH-VALUE                                     
410000       MOVE JA TO TRANS-RIKTIG                                            
420000     ELSE                                                                 
430000       MOVE NEJ TO TRANS-RIKTIG                                           
440000       MOVE +1 TO FELTAB-INDEX                                            
450007       PERFORM UNTIL TRANS-FELKOD (FELTAB-INDEX) = HIGH-VALUE             
460000         MOVE V165-IDDEL TO W-FEL-IDDEL                                   
470000         MOVE +0 TO W-KDFELMRK                                            
480000         MOVE TRANS-FELKOD (FELTAB-INDEX) TO W-IDFELKODX                  
490000         MOVE TRANS-FELTEXT (FELTAB-INDEX) TO W-FELTEXT                   
500000         PERFORM S30-SKRIV-FELFIL                                         
510000         ADD +1 TO FELTAB-INDEX                                           
520000       END-PERFORM                                                        
530000     END-IF                                                               
540007     .                                                                    
541027     EJECT                                                                
550013 CC-BEH-KORREKT-R89   SECTION.                                            
560013     SKIP2                                                                
570013******************************************************************        
580013*    -- R89:OR SKA MED I STATISTIKEN OCH KONTROLLISTAN,          *        
590013*    -- MEN INGEN BEHANDLING I ÖVRIGT EFTERSOM ANDRA TRANSAR     *        
600013*    -- GENERERATS I DERAS STÄLLE.                               *        
610013******************************************************************        
620013                                                                          
630017     IF V165-TRANSTYP NOT = SPACE AND 'R89'                               
640014*      -- FÖRSTA R89:AN. TA HAND OM FÖREGÅENDE TRANSTYP                   
650013       CANCEL REGELMODUL                                                  
660013       MOVE JA TO NY-TRANSTYP                                             
670013       MOVE V165-TRANSTYP TO SPARAD-TRANSTYP                              
680017       MOVE 'R89' TO V165-TRANSTYP                                        
690013     END-IF                                                               
700013                                                                          
710013     MOVE ZERO TO W-KONTROLL-IDDEL                                        
720013     INSPECT R89-IDARTNR REPLACING ALL SPACE BY ZERO                      
730013     MOVE R89-KDCLAGER TO KDCLAGER                                        
740013     MOVE R89-IDARTNR TO SORTBGP                                          
750013     MOVE IN-KORT TO W-KONTROLL-POST                                      
760013                                                                          
770013     MOVE JA TO TRANS-RIKTIG                                              
780013     .                                                                    
790000     EJECT                                                                
800000 D-FINALEN SECTION.                                                       
810000     SKIP2                                                                
820007     CLOSE INFIL                                                          
840007           W09227                                                         
860007     .                                                                    
870000     EJECT                                                                
880000 S10-LAES-INFIL SECTION.                                                  
890000     SKIP2                                                                
900000     READ INFIL INTO W-IN-TRANS                                           
910000     AT END MOVE JA TO EOF-INFIL                                          
920000     END-READ                                                             
930007     .                                                                    
940000     SKIP2                                                                
950000 S15-RETURN-SORTFIL SECTION.                                              
960000     SKIP2                                                                
970000     RETURN SORTFIL INTO IN-TRANS                                         
980000     AT END MOVE JA TO EOF-SORTFIL                                        
990000     END-RETURN                                                           
000007     .                                                                    
010000     EJECT                                                                
020000 S20-CALL-REGELMODUL SECTION.                                             
030000     SKIP2                                                                
040007     CALL REGELMODUL USING IN-KORT,                                       
050000                           V165-TRANS,                                    
060000                           V165-FELAREA,                                  
070000                           V165-RETURKOD                                  
080007     .                                                                    
090000     EJECT                                                                
100000 S25-KOLLA-IDDEL SECTION.                                                 
110000     SKIP2                                                                
120000     IF V165-IDDISTR NOT NUMERIC                                          
130000       MOVE +0 TO V165-IDDISTR                                            
140000     END-IF                                                               
150000     IF V165-IDKUNDNR NOT NUMERIC                                         
160000       MOVE +0 TO V165-IDKUNDNR                                           
170000     END-IF                                                               
180000     IF V165-KDCLAGER NOT NUMERIC                                         
190000       MOVE +0 TO V165-KDCLAGER                                           
200000     END-IF                                                               
210000     IF V165-IDPTYP = 'R14' OR 'R15'                                      
220000       CONTINUE                                                           
230000     ELSE                                                                 
240000       IF V165-KDFRAKT NOT NUMERIC                                        
250000         MOVE +0 TO V165-KDFRAKT                                          
260000       END-IF                                                             
270000       IF V165-IDORDNR NOT NUMERIC                                        
280000         MOVE +0 TO V165-IDORDNR                                          
290000       END-IF                                                             
300000     END-IF                                                               
310000     IF V165-KDORDKL NOT NUMERIC                                          
320000       MOVE +0 TO V165-KDORDKL                                            
330000     END-IF                                                               
340000     IF V165-SORTBGP NOT NUMERIC                                          
350000       MOVE +0 TO V165-SORTBGP                                            
360000     END-IF                                                               
370007     .                                                                    
380000     EJECT                                                                
390000 S30-SKRIV-FELFIL SECTION.                                                
400000     SKIP2                                                                
410000******************************************************************        
420000*    I DENNA SECTION SKRIVS FELAKTIGA TRANSAKTIONER PÅ           *        
430006*    FELFILEN W09227. TRANSAKTIONERNA BESTÅR AV EN 36            *        
440000*    TECKEN LÅNG IDENTIFIKATIONS-DEL OCH EN 80 TECKEN            *        
450007*    LÅNG AREA SOM INNEHÅLLER FELTEXTEN.                         *        
460000******************************************************************        
470000     SKIP2                                                                
480000     IF W-IDPTYP = 'R05'                                                  
490000       MOVE R05-GLURPKOD TO W-FELTEXT                                     
500000     ELSE                                                                 
510007       IF W-IDPTYP = 'R22' AND R22-IDBESTNR = SPACE                       
520007         MOVE +1 TO W-KDFELMRK                                            
530007       END-IF                                                             
540000     END-IF                                                               
550007                                                                          
560000     WRITE FEL-POST FROM W-FEL-POST                                       
570007     .                                                                    
580000     EJECT                                                                
720000 S60-CALL-TRANSMODUL SECTION.                                             
730000     SKIP2                                                                
740000******************************************************************        
750007*    I DENNA SECTION ANROPAS EN BEHANDLANDE TRANS-SUBMODUL FÖR   *        
760007*    ATT SKRIVA UT OCH EV YTTERLIGARE KONTROLLERA EN FORMELLT    *        
770007*    GODKÄND TRANS.                                              *        
780000*    NAMNET PÅ TRANSMODULEN FINNS I FÄLTET "INLADDAD-MODUL"-     *        
790007*    FELTABELLENS FÖRSTA TRE BYTES INITIERAS MED HIGH-VALUE.     *        
800007*    OM DETTA ÄR ÄNDRAT VID ÅTERKOMSTEN SÅ INNEHÅLLER TRANSEN    *        
810007*    LOGISKA FEL OCH SKA BEHANDLAS SOM FELAKTIG.                 *        
820000******************************************************************        
830000     SKIP2                                                                
840000     MOVE HIGH-VALUE TO TRANS-FELKOD (1)                                  
850000     CALL INLADDAD-MODUL USING TRANS-PARM                                 
860007     .                                                                    
870000     EJECT                                                                
880000 S70-LAES-EXTTAB SECTION.                                                 
890000     SKIP2                                                                
900000     READ EXTTAB INTO W-EXTTAB                                            
910000     AT END MOVE JA TO EOF-EXTTAB                                         
920000     END-READ                                                             
930007     .                                                                    
940017     EJECT                                                                
950018 S80-DISP-W09206-001-RUBRIKER SECTION.                                    
960021                                                                          
970031     DISPLAY  'VOLVO CAR PARTS  W09206-001     '                          
980017              '       AVSTÄMNINGSLISTA   DAGENS TRANSAKTIONER'            
990017              '       DATUM ' W-DATUM.                                    
000017     DISPLAY  ' '                                                         
010017     DISPLAY  '    KORTTYP  KORREKTA  FELAKTIGA  TOTALT ANTAL'            
020017     DISPLAY  ' '                                                         
030017     .                                                                    
040017     SKIP3                                                                
050018 S81-DISP-W09206-001-TRANSSUM SECTION.                                    
060021                                                                          
070017     MOVE RIKTIGA-DENNA-TRANSTYP   TO DIS-RIKTIGA                         
080017     MOVE FELAKTIGA-DENNA-TRANSTYP TO DIS-FELAKTIGA                       
090017     ADD RIKTIGA-DENNA-TRANSTYP                                           
100017         FELAKTIGA-DENNA-TRANSTYP  GIVING DIS-TOTALT                      
110017     DISPLAY   '    ' SPARAD-TRANSTYP                                     
120017              '     ' DIS-RIKTIGA                                         
130017                 '  ' DIS-FELAKTIGA                                       
140020              '     ' DIS-TOTALT                                          
150017     MOVE NEJ TO NY-TRANSTYP                                              
160017                                                                          
170017     ADD RIKTIGA-DENNA-TRANSTYP TO TOT-RIKTIGA                            
180017     ADD FELAKTIGA-DENNA-TRANSTYP TO TOT-FELAKTIGA                        
190017     MOVE ZERO TO RIKTIGA-DENNA-TRANSTYP                                  
200017     MOVE ZERO TO FELAKTIGA-DENNA-TRANSTYP                                
210017     .                                                                    
220017     SKIP3                                                                
230018 S82-DISP-W09206-001-TOTSUM SECTION.                                      
240021                                                                          
250017     MOVE TOT-RIKTIGA                    TO DIS-RIKTIGA                   
260017     MOVE TOT-FELAKTIGA                  TO DIS-FELAKTIGA                 
270017     ADD  TOT-RIKTIGA TOT-FELAKTIGA  GIVING DIS-TOTALT                    
280017     DISPLAY ' '                                                          
290020     DISPLAY   '    ' '***'                                               
300017              '     ' DIS-RIKTIGA                                         
310017                 '  ' DIS-FELAKTIGA                                       
320020              '     ' DIS-TOTALT                                          
330019     .                                                                    
340021     EJECT                                                                
350022 S90-DISP-W09206-002-RUBRIKER SECTION.                                    
360021                                                                          
361024     DISPLAY  ' '                                                         
362024     DISPLAY  ' '                                                         
363024     DISPLAY  ' '                                                         
364024     DISPLAY  ' '                                                         
370031     DISPLAY  'VOLVO CAR PARTS  W09206-002     '                          
380021              '       TRANSAKTIONER SOM SAKNAR KONTROLLMODUL '            
390021              '       DATUM ' W-DATUM.                                    
400021     DISPLAY  ' '                                                         
410021     DISPLAY  '    KORTTYP     ANTAL  KORTBILD'                           
420021     DISPLAY  ' '                                                         
430021     .                                                                    
440021     SKIP3                                                                
450022 S91-DISP-W09206-002-DETRAD SECTION.                                      
460021     SKIP3                                                                
470024     DISPLAY  '    ' IN-IDKTYP  '                ' IN-KORT (1:80)         
480021     ADD 1 TO FELAKTIGA-DENNA-TRANSTYP                                    
490021     .                                                                    
500021     SKIP3                                                                
510022 S92-DISP-W09206-002-TRANSSUM SECTION.                                    
520021                                                                          
530021     MOVE FELAKTIGA-DENNA-TRANSTYP TO DIS-FELAKTIGA                       
550021     DISPLAY   '    ' SPARAD-TRANSTYP                                     
560021              '     ' DIS-FELAKTIGA                                       
570025     DISPLAY ' '                                                          
580021                                                                          
590021     ADD FELAKTIGA-DENNA-TRANSTYP TO TOT-FELAKTIGA                        
600021     MOVE ZERO TO FELAKTIGA-DENNA-TRANSTYP                                
610021     .                                                                    
620021     SKIP3                                                                
630022 S93-DISP-W09206-002-TOTSUM SECTION.                                      
640021                                                                          
650021     MOVE TOT-FELAKTIGA TO DIS-FELAKTIGA                                  
660021     DISPLAY ' '                                                          
670021     DISPLAY   '    ' '***'                                               
680021              '     ' DIS-FELAKTIGA                                       
690021     .                                                                    
