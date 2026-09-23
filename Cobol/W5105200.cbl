000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5105200.                                                
000400 AUTHOR.         KARL JOHAN HANSSON.                                      
000500 DATE-WRITTEN.   JANUARI 1997                                             
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*      - LÄSER WDK7 MED SB                                                
001000*      - MEN FÖRST, LÄS IGENOM HELA DC-BASEN WDB6                         
001100*      - SKAPAR SORTFIL MED SAMTLIGA SALDON FÖR SDC OCH NDC               
001200*      - ALLA POSTER SORTERAS PÅ ARTIKELNR I EN COBOL-SORT.               
001300*      - I OUTPUT PROCEDURE SKER EN FÖRDELNING AV POSTERNA                
001400*        PÅ TVÅ FILER EN FÖR SDC OCH NDC PACIFIC OCH                      
001500*        EN FÖR NDC NORDAMERIKA (LAB).                                    
001600*        SAMT EN FÖR NDC CHINA.                                           
001610*        SAMT EN FÖR NDC INDIA.                                           
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- SAMTLIGA ARTIKLARS SALDO FÖR LAB-IDDC                      
002500     SELECT W51052                     ASSIGN TO W51052D1.                
002600*          --- SAMTLIGA ARTIKLARS SALDO FÖR SDC-IDDC                      
002700     SELECT W51053                     ASSIGN TO W51052D2.                
002800*          --- SAMTLIGA ARTIKLARS SALDO                                   
002900     SELECT W5105C                     ASSIGN TO W51052D3.                
003000*          --- SORTERINGSFIL                                              
003010     SELECT W5105D                     ASSIGN TO W51052D4.                
003020*          --- SORTERINGSFIL                                              
003100     SELECT SORTFIL                    ASSIGN TO W51052DS.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W51052                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  UT-LAB -COPY W51052  -L.                                             
004200     SKIP3                                                                
004300 FD  W51053                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  UT-SDC -COPY W51054  -L.                                             
004800     SKIP3                                                                
004900 FD  W5105C                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  UT-CN  -COPY W5705C  -L.                                             
005400     SKIP3                                                                
005410 FD  W5105D                                                               
005420     RECORDING       F                                                    
005430     BLOCK CONTAINS  0.                                                   
005440                                                                          
005450*01  UT-IN  -COPY W5705C  -L.                                             
005460     SKIP3                                                                
005500 SD  SORTFIL.                                                             
005600                                                                          
005700 01  SRT-POST.                                                            
005800*    03  -COPY W51052 -PRE  SRT-                                          
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100                                                                          
006200 77  IDPGM                       PIC X(8)    VALUE 'W5105200'.            
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500                                                                          
007800 01  ARBETSAREOR.                                                         
007900     03 WS-SPAR-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.           
008000                                                                          
008100 01  SWITCHAR.                                                            
008200     03 SORT-EOF                 PIC X       VALUE 'N'.                   
008300                                                                          
008400 01  DYNAMISKA-SUBPROGRAM.                                                
008500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009000     SKIP2                                                                
009100*    --- PARAMETRAR TILL ABEND                                            
009200                                                                          
009300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009500     SKIP2                                                                
009600 01  FELTEXT.                                                             
009700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009900     EJECT                                                                
010000                                                                          
010100*    --- PARAMETRAR TILL POSTSUM                                          
010200*                                                                         
010300*01  -COPY W0005   -PRE  POSTSUM-                                         
010400     EJECT                                                                
010500 01  FILLER                      PIC X(24)   VALUE 'SORTWS-AREA'.         
010600                                                                          
010700 01  AREA   -COPY W51052 -PRE SORTWS-                                     
010800 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(24)   VALUE 'UT53-AREA'.           
011200 01  AREA   -COPY W51054 -PRE UT53-                                       
011300     EJECT                                                                
011301                                                                          
011310 01  FILLER                      PIC X(24)   VALUE 'UT5C-AREA'.           
011330 01  AREA   -COPY W5705C -PRE UT5C-                                       
011340     EJECT                                                                
011341                                                                          
011350 01  FILLER                      PIC X(24)   VALUE 'UT5D-AREA'.           
011360 01  AREA   -COPY W5705C -PRE UT5D-                                       
011370     EJECT                                                                
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  BASEN-SLUT                          VALUE 'GB'.                  
012200     SKIP2                                                                
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNKTIONSKODER                                               
012900*01  -COPY W0003                                                          
013000     EJECT                                                                
013010 01 KEYS-FOR-DLI.                                                         
013020    03  W-IDDC-B6-X.                                                      
013030        05 W-IDDC-B6        PIC X(2) VALUE SPACES.                        
013040                                                                          
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013300     SKIP3                                                                
013400 01  DLI-IO-AREA.                                                         
013500     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
013600     SKIP3                                                                
013700     03  WDK701   REDEFINES IO-AREA.                                      
013800*        05  -COPY WDK701                                                 
013900     SKIP3                                                                
014000     03  WDK711   REDEFINES IO-AREA.                                      
014100*        05  -COPY WDK711                                                 
014200                                                                          
014300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014400 01   DLI-IO-AREA-B601.                                                   
014500*     03  -COPY WDB601                                                    
014600                                                                          
014700     EJECT                                                                
014800 LINKAGE SECTION.                                                         
014900*01  -COPY W0008  -PRE WDK7-                                              
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200                                                                          
015300*01  -COPY W0008      -PRE WDB6-                                          
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600                                                                          
015700 PROCEDURE DIVISION  USING WDK7-PCB WDB6-PCB.                             
015800 MAIN SECTION.                                                            
015900     ENTRY 'DLITCBL' USING WDK7-PCB WDB6-PCB.                             
016000                                                                          
016100     PERFORM A-INIT                                                       
016200                                                                          
016300     SORT SORTFIL ASCENDING KEY SRT-IDARTNR                               
016400                  INPUT  PROCEDURE B-LAES-SKAPA-SALDOPOSTER               
016500                  OUTPUT PROCEDURE C-SKRIV-LAB-POSTER                     
016600                                                                          
016700     IF SORT-RETURN NOT = 0                                               
016800       MOVE SORT-RETURN TO SORT-RETURN-X                                  
016900       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
017000           DELIMITED BY SIZE                                              
017100           INTO FELTEXT-STR                                               
017200       DISPLAY FELTEXT                                                    
017300       PERFORM S99-ABEND                                                  
017400     END-IF                                                               
017500                                                                          
017600     PERFORM Z-FINIT                                                      
017700                                                                          
017800     MOVE ZERO TO RETURN-CODE                                             
017900     GOBACK                                                               
018000     .                                                                    
018100     EJECT                                                                
018200 A-INIT SECTION.                                                          
018300                                                                          
018400     OPEN OUTPUT W51052                                                   
018500                 W51053                                                   
018600                 W5105C                                                   
018610                 W5105D                                                   
018700                                                                          
018800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020400     .                                                                    
020500     EJECT                                                                
020600 B-LAES-SKAPA-SALDOPOSTER SECTION.                                        
020700                                                                          
020800     PERFORM IMS-GET-WDK7                                                 
020900     PERFORM UNTIL BASEN-SLUT                                             
021000        EVALUATE WDK7-SEG-NAME-FB                                         
021100           WHEN 'WDK701  '                                                
021200              MOVE SART-IDARTNR TO WS-SPAR-IDARTNR                        
021300           WHEN 'WDK711  '                                                
021401              MOVE SLAG-IDDC    TO W-IDDC-B6                              
021402              PERFORM IMS-GU-WDB601                                       
021403              IF DCS-NDC-NA                                               
021420                PERFORM BC-SKAPA-NDC-POSTER                               
021430              ELSE                                                        
021512                IF DCS-LAND-NON-VCC-OWNED                                 
021532                  IF DCS-INDIA                                            
021533                    PERFORM BE-SKAPA-IN-POSTER                            
021534                  ELSE                                                    
021535                    PERFORM BD-SKAPA-SC-POSTER                            
021536                  END-IF                                                  
021553                ELSE                                                      
021554                  PERFORM BB-SKAPA-SDC-POSTER                             
021570                END-IF                                                    
021700              END-IF                                                      
022100         END-EVALUATE                                                     
022200         PERFORM IMS-GET-WDK7                                             
022300     END-PERFORM                                                          
022400     .                                                                    
022500     SKIP3                                                                
024000 BB-SKAPA-SDC-POSTER SECTION.                                             
024100                                                                          
024200     MOVE FUNCTION CURRENT-DATE(01:8) TO UT53-DAREGDAT                    
024300     MOVE FUNCTION CURRENT-DATE(09:8) TO UT53-TIKLOCK                     
024400     MOVE WS-SPAR-IDARTNR             TO UT53-IDARTNR                     
024500     MOVE SLAG-IDDC                   TO UT53-IDDC                        
024600     MOVE SLAG-KVLS                   TO UT53-KVLS                        
024700     MOVE SLAG-KVEFRS                 TO UT53-KVEFRS                      
024800     MOVE SLAG-KVAKS-SDC              TO UT53-KVAKS                       
024900     MOVE SLAG-KVAKS-PAV              TO UT53-KVAKS-PAV                   
025000     MOVE ZERO                        TO UT53-PRINK                       
025100                                         UT53-PRARTSTD                    
025200                                                                          
025300     PERFORM S02-SKRIV-W51053-FIL                                         
025400     .                                                                    
025500     SKIP3                                                                
025600 BC-SKAPA-NDC-POSTER SECTION.                                             
025700                                                                          
025800     MOVE WS-SPAR-IDARTNR     TO SORTWS-IDARTNR                           
025900     MOVE SLAG-IDDC           TO SORTWS-IDDC                              
026000     MOVE SLAG-KVLS           TO SORTWS-KVLS                              
026100     MOVE SLAG-KVEFRS         TO SORTWS-KVEFRS                            
026200     MOVE SLAG-KVAKS-SDC      TO SORTWS-KVAKS-SDC                         
026300     MOVE SLAG-KVAKS-PAV      TO SORTWS-KVAKS-PAV                         
026400     MOVE SLAG-PRAVCOST       TO SORTWS-PRAVCOST                          
026500                                                                          
026600     PERFORM S01-RELEASE-SORTPOST                                         
026700     .                                                                    
026800     EJECT                                                                
026801                                                                          
026810 BD-SKAPA-SC-POSTER SECTION.                                              
026811* SC- SALES COMPANY                                                       
026812     MOVE FUNCTION CURRENT-DATE(01:8) TO UT5C-DAREGDAT                    
026813     MOVE FUNCTION CURRENT-DATE(09:8) TO UT5C-TIKLOCK                     
026820     MOVE WS-SPAR-IDARTNR             TO UT5C-IDARTNR                     
026830     MOVE SLAG-IDDC                   TO UT5C-IDDC                        
026840     MOVE SLAG-KVLS                   TO UT5C-KVLS                        
026850     MOVE SLAG-KVEFRS                 TO UT5C-KVEFRS                      
026860     MOVE SLAG-KVAKS-SDC              TO UT5C-KVAKS                       
026870     MOVE SLAG-KVAKS-PAV              TO UT5C-KVAKS-PAV                   
026880     MOVE SLAG-PRAVCOST               TO UT5C-PRAVCOST                    
026890     MOVE DCS-KDTRADP                 TO UT5C-KDTRADP                     
026894                                                                          
026895     PERFORM S02-SKRIV-W5105C-FIL                                         
026896     .                                                                    
026897     SKIP3                                                                
026898 BE-SKAPA-IN-POSTER SECTION.                                              
026899     MOVE FUNCTION CURRENT-DATE(01:8) TO UT5D-DAREGDAT                    
026900     MOVE FUNCTION CURRENT-DATE(09:8) TO UT5D-TIKLOCK                     
026901     MOVE WS-SPAR-IDARTNR             TO UT5D-IDARTNR                     
026902     MOVE SLAG-IDDC                   TO UT5D-IDDC                        
026903     MOVE SLAG-KVLS                   TO UT5D-KVLS                        
026904     MOVE SLAG-KVEFRS                 TO UT5D-KVEFRS                      
026905     MOVE SLAG-KVAKS-SDC              TO UT5D-KVAKS                       
026906     MOVE SLAG-KVAKS-PAV              TO UT5D-KVAKS-PAV                   
026907     MOVE SLAG-PRAVCOST               TO UT5D-PRAVCOST                    
026908     MOVE DCS-KDTRADP                 TO UT5D-KDTRADP                     
026909                                                                          
026910     PERFORM S02-SKRIV-W5105D-FIL                                         
026911     .                                                                    
026912     SKIP3                                                                
026930 C-SKRIV-LAB-POSTER SECTION.                                              
027000                                                                          
027100     PERFORM S03-RETURN-SORTPOST                                          
027200     PERFORM UNTIL SORT-EOF = JA                                          
027300       PERFORM S04-SKRIV-LAB-POST                                         
027400       PERFORM S03-RETURN-SORTPOST                                        
027500     END-PERFORM                                                          
027600     .                                                                    
027700     EJECT                                                                
027800 Z-FINIT SECTION.                                                         
027900                                                                          
028000     CLOSE W51052                                                         
028100           W51053                                                         
028110           W5105C                                                         
028120           W5105D                                                         
028200                                                                          
028300     MOVE 'S'        TO POSTSUM-OPKOD                                     
028400     CALL POSTSUM USING POSTSUM-PARM                                      
028500     .                                                                    
028600     EJECT                                                                
028700 S01-RELEASE-SORTPOST SECTION.                                            
028800                                                                          
028900     RELEASE SRT-POST FROM SORTWS-AREA                                    
029000                                                                          
029100     MOVE 'SORTIN'   TO POSTSUM-FDNAMN                                    
029200     MOVE 'W51052DS' TO POSTSUM-DDNAMN2                                   
029300     CALL POSTSUM USING POSTSUM-PARM                                      
029400     .                                                                    
029500     EJECT                                                                
029600 S02-SKRIV-W51053-FIL SECTION.                                            
029700                                                                          
029800     WRITE UT-SDC     FROM UT53-AREA                                      
029900                                                                          
030000     MOVE 'W51053'   TO POSTSUM-FDNAMN                                    
030100     MOVE 'W51052D2' TO POSTSUM-DDNAMN2                                   
030200     CALL POSTSUM USING POSTSUM-PARM                                      
030300     .                                                                    
030400     EJECT                                                                
030410 S02-SKRIV-W5105C-FIL SECTION.                                            
030420                                                                          
030430     WRITE UT-CN      FROM UT5C-AREA                                      
030440                                                                          
030450     MOVE 'W5105C'   TO POSTSUM-FDNAMN                                    
030460     MOVE 'W51052D3' TO POSTSUM-DDNAMN2                                   
030470     CALL POSTSUM USING POSTSUM-PARM                                      
030480     .                                                                    
030490     EJECT                                                                
030491 S02-SKRIV-W5105D-FIL SECTION.                                            
030492                                                                          
030493     WRITE UT-IN      FROM UT5D-AREA                                      
030494                                                                          
030495     MOVE 'W5105D'   TO POSTSUM-FDNAMN                                    
030496     MOVE 'W51052D4' TO POSTSUM-DDNAMN2                                   
030497     CALL POSTSUM USING POSTSUM-PARM                                      
030498     .                                                                    
030499     EJECT                                                                
030510 S03-RETURN-SORTPOST SECTION.                                             
030600                                                                          
030700     RETURN SORTFIL INTO SORTWS-AREA                                      
030800     AT END                                                               
030900       MOVE JA         TO SORT-EOF                                        
031000     NOT AT END                                                           
031100       MOVE 'SORTUT'   TO POSTSUM-FDNAMN                                  
031200       MOVE 'W51052DS' TO POSTSUM-DDNAMN2                                 
031300       CALL POSTSUM USING POSTSUM-PARM                                    
031400     END-RETURN                                                           
031500     .                                                                    
031600     EJECT                                                                
031700 S04-SKRIV-LAB-POST SECTION.                                              
031800                                                                          
031900     WRITE UT-LAB FROM SORTWS-AREA                                        
032000                                                                          
032100     MOVE 'W51052'   TO POSTSUM-FDNAMN                                    
032200     MOVE 'W51052D1' TO POSTSUM-DDNAMN2                                   
032300     CALL POSTSUM USING POSTSUM-PARM                                      
032400     .                                                                    
032500     EJECT                                                                
032600 S99-ABEND SECTION.                                                       
032700                                                                          
032800     MOVE 'S' TO POSTSUM-OPKOD                                            
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
033100     .                                                                    
033200     EJECT                                                                
033300* --- IMS SEKTIONER ---                                                   
033400     SKIP3                                                                
033500 IMS-GET-WDK7   SECTION.                                                  
033600                                                                          
033700     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA                           
033800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
033900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
034000     PERFORM IMS-STATUSKONTROLL                                           
034100     .                                                                    
034200     SKIP3                                                                
034300 IMS-GU-WDB601    SECTION.                                                
034400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
034410          DELIMITED BY SIZE INTO SSA1                                     
034500     MOVE '  '       TO GODK-STATUSKODER                                  
034600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
034700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
034800     PERFORM IMS-STATUSKONTROLL                                           
034900     .                                                                    
035000     SKIP3                                                                
035100 IMS-STATUSKONTROLL SECTION.                                              
035200                                                                          
035300     SET STATUS-IX TO 1                                                   
035400     SEARCH GODK-STATUS                                                   
035500       AT END                                                             
035600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
035700         DISPLAY FELTEXT                                                  
035800         CALL FELLOG                                                      
035900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036000         CONTINUE                                                         
036100     END-SEARCH                                                           
036200     .                                                                    
