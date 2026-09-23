010001 ID DIVISION.                                                             
020001 PROGRAM-ID.     W2368600.                                                
030000 AUTHOR.         BODIL LINDAHL.                                           
040001 DATE-WRITTEN.   MARS 2007                                                
050000 DATE-COMPILED.                                                           
060000                                                                          
070000                                                                          
080000*    FUNKTION:                                                            
090000*        NEDLÄSNING HTR WDR301                                            
100001*        SPFU MANUAL UPDATE                                               
110000*                                                                         
120000*        PROGRAMMET LÄSER WDR301                                          
130001*                                                                         
140001*        SKAPAR FIL W23686 SPFU MANUAL UPDATE                             
150001*                   W23687 FÖR BORTTAG HTR                                
160000*                                                                         
170000                                                                          
180000 ENVIRONMENT DIVISION.                                                    
190000                                                                          
200000 INPUT-OUTPUT SECTION.                                                    
210000                                                                          
220000 FILE-CONTROL.                                                            
230000                                                                          
240002*          --- FIL SPFU                                                   
250002     SELECT W23686                     ASSIGN TO W23686D1.                
260000*          --- BORTTAG HTR                                                
270002     SELECT W23687                     ASSIGN TO W23686D2.                
280000     EJECT                                                                
290000 DATA DIVISION.                                                           
300000     SKIP3                                                                
310000 FILE SECTION.                                                            
320000                                                                          
330002 FD  W23686                                                               
340000     RECORDING       F                                                    
350000     BLOCK CONTAINS  0.                                                   
360003*01  POST -COPY W23684  -PRE  UT-     -L.                                 
370000     EJECT                                                                
380003 FD  W23687                                                               
390000     RECORDING       F                                                    
400000     BLOCK CONTAINS  0.                                                   
410000*01  POST -COPY WDR301  -PRE  BORT-   -L.                                 
420000     EJECT                                                                
430000 WORKING-STORAGE SECTION.                                                 
440000*    -- CHECKED BY WY2000                                                 
450000                                                                          
460004 77  IDPGM                       PIC X(8)    VALUE 'W2368600'.            
470000 77  JA                          PIC X       VALUE 'J'.                   
480000 77  NEJ                         PIC X       VALUE 'N'.                   
       77  NOLL-RAKNARE                PIC S9(5)  COMP-3 VALUE ZERO.            
       77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
510000                                                                          
520000 01  FELTEXT.                                                             
530000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
540000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
550000                                                                          
561020 01  WS-HHMMSSTH.                                                         
564020     03  WS-HH                   PIC 9(2).                                
565020     03  WS-MM                   PIC 9(2).                                
566020     03  WS-SS                   PIC 9(2).                                
568020     03  WS-TH                   PIC 9(2).                                
570004                                                                          
561020 01  WS-HHMMSSTH-D.                                                       
564020     03  WS-HH-D                 PIC 9(2).                                
565020     03  WS-MM-D                 PIC 9(2).                                
566020     03  WS-SS-D                 PIC 9(2).                                
568020     03  WS-TH-D                 PIC 9(2).                                
570004                                                                          
570120 01  WS-TIME.                                                             
570220     03  WS-TIME-HH              PIC 9(2).                                
570321     03  WS-PKT1                 PIC X VALUE ':'.                         
570420     03  WS-TIME-MM              PIC 9(2).                                
570521     03  WS-PKT2                 PIC X VALUE ':'.                         
570620     03  WS-TIME-SS              PIC 9(2).                                
                                                                                
570120 01  WS-TIME-D.                                                           
570220     03  WS-TIME-HH-D            PIC 9(2).                                
570321     03  WS-PKT1-D               PIC X VALUE ':'.                         
570420     03  WS-TIME-MM-D            PIC 9(2).                                
570521     03  WS-PKT2-D               PIC X VALUE ':'.                         
570620     03  WS-TIME-SS-D            PIC 9(2).                                
570820                                                                          
576009 01  WS-TIAAMMDD                 PIC 9(6).                                
576009 01  FILLER REDEFINES WS-TIAAMMDD.                                        
577009     03  WS-AA1                  PIC 9(2).                                
578009     03  WS-MM1                  PIC 9(2).                                
579009     03  WS-DD1                  PIC 9(2).                                
579109                                                                          
579209 01  WS-DDMMAAAA.                                                         
579309     03  WS-DD2                  PIC 9(2).                                
579409     03  SNED1                   PIC X VALUE '/'.                         
579509     03  WS-MM2                  PIC 9(2).                                
579609     03  SNED1                   PIC X VALUE '/'.                         
579709     03  WS-SS2                  PIC 9(2).                                
579809     03  WS-AA2                  PIC 9(2).                                
579909                                                                          
580016 01  WS-CCYYMMDD.                                                         
580117     03  WS-CC                   PIC 9(2).                                
580217     03  WS-YY                   PIC 9(2).                                
580318     03  WS-STRECK1              PIC X VALUE '-'.                         
580421     03  WS-MM3                  PIC 9(2).                                
580520     03  WS-STRECK2              PIC X VALUE '-'.                         
580622     03  WS-DD                   PIC 9(2).                                
580716                                                                          
581000 01  DYNAMISKA-SUBPROGRAM.                                                
590000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
600000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
610000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
611010     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
620000     EJECT                                                                
621011*    --- PARAMETRAR TILL DATKORT                                          
623011 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23686'.              
624011     SKIP2                                                                
625011 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
626011     SKIP2                                                                
627011*01  -COPY WDATKORT                                                       
628011     EJECT                                                                
630000*    --- PARAMETRAR TILL POSTSUM                                          
640000*01  -COPY W0005   -PRE  POSTSUM-                                         
650001*                                                                         
660000     EJECT                                                                
670000 01  UT-AREA-START               PIC X(16)   VALUE                        
680000                                             'UT-AREA-START'.             
690004*01  AREA -COPY W23684      -PRE UT-                                      
700000     EJECT                                                                
710022*01  AREA -COPY W23686      -PRE WS-UT-                                   
720006     EJECT                                                                
730000 01  BORT-AREA-START             PIC X(16)   VALUE                        
740000                                             'BORT-AREA-START'.           
750000*01  AREA -COPY WDR301      -PRE BORT-                                    
760000     EJECT                                                                
770000 01  NYCKLAR-TILL-DLI.                                                    
780000     03  W-WDR301KY-X.                                                    
790000         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
800000     03  W-IDCPYTXT-X.                                                    
810025         05  W-IDCPYTXT          PIC X(08)    VALUE 'W23686  '.           
890000                                                                          
900000*    --- STATUS-KOD FRÅN IMS                                              
910000 01  STATUS-WS                   PIC XX.                                  
920000     88  SEGMENT-FINNS                       VALUE '  '.                  
930000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
940000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
950000     88  IMS-EJ-OK                           VALUE 'XD'.                  
960000                                                                          
970000 01  GODK-STATUSKODER.                                                    
980000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010001 01  SSA1                        PIC X(96).                               
020000 01  SSA2                        PIC X(64).                               
030000     EJECT                                                                
040000*    --- IMS FUNKTIONSKODER                                               
050000*01  -COPY W0003                                                          
060000     EJECT                                                                
070000*    ---  DLI INPUT-OUTPUT AREA                                           
080000                                                                          
090001 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDR301'.                    
100001 01  DLI-IO-WDR301.                                                       
110000*  03  -COPY WDR301                                                       
130000     EJECT                                                                
220000 LINKAGE SECTION.                                                         
230001*01  -COPY W0008  -PRE WDR3-                                              
240000     05  FILLER                  PIC X.                                   
250001     EJECT                                                                
290004 PROCEDURE DIVISION  USING WDR3-PCB.                                      
300000 MAIN SECTION.                                                            
310004     ENTRY 'DLITCBL' USING WDR3-PCB.                                      
320000                                                                          
330000     PERFORM A-INIT                                                       
340000                                                                          
350000     PERFORM IMS-GET-FILC-ROT                                             
360000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
370000                                                                          
380006        MOVE FIL-WDR301-DATA TO WS-UT-AREA                                
390001        PERFORM B-SKAPA-SKRIV-UTPOST                                      
400001        PERFORM C-SKAPA-SKRIV-BORTPOST                                    
410001        PERFORM S10-NOLLSTALL                                             
420000                                                                          
430000        PERFORM IMS-GET-FILC-ROT                                          
440000     END-PERFORM                                                          
450000                                                                          
460000     PERFORM Z-FINIT                                                      
470000     MOVE ZERO TO RETURN-CODE                                             
480000     GOBACK                                                               
490000     .                                                                    
500000     EJECT                                                                
510000 A-INIT SECTION.                                                          
520000                                                                          
530004     OPEN OUTPUT W23686                                                   
540004                 W23687                                                   
541020     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
550000                                                                          
551012     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
551118     MOVE 20       TO WS-CC                                               
552018     MOVE D-AAR    TO WS-YY                                               
553021     MOVE D-MAANAD TO WS-MM3                                              
554018     MOVE D-DAG    TO WS-DD                                               
554120     MOVE WS-CCYYMMDD TO UT-STAT-DATE                                     
554320                                                                          
555020     ACCEPT WS-HHMMSSTH FROM TIME                                         
556020     MOVE WS-HH    TO WS-TIME-HH                                          
580120     MOVE WS-MM    TO WS-TIME-MM                                          
580220     MOVE WS-SS    TO WS-TIME-SS                                          
580322     MOVE WS-TIME  TO UT-STAT-TIME                                        
580420                                                                          
581012     MOVE 'SPFU5'         TO UT-FILE-TYPE                                 
582012     MOVE 'BP2TW'         TO UT-PLANT                                     
583012     MOVE 0               TO UT-SUP-UNIT                                  
590001     PERFORM S10-NOLLSTALL                                                
600000     .                                                                    
610000     EJECT                                                                
620001 B-SKAPA-SKRIV-UTPOST SECTION.                                            
630001                                                                          
640006     MOVE WS-UT-IDLEVNR   TO UT-IDLEVNR                                   
650006     MOVE WS-UT-KVAVIS    TO UT-ORDERED                                   
651008     MOVE WS-UT-KVANTMOT  TO UT-DELIVERED                                 
651108     MOVE WS-UT-TYP       TO UT-STAT                                      
                                                                                
           MOVE ZERO TO NOLL-RAKNARE                                            
           MOVE WS-UT-IDARTNR TO WS-IDARTNR                                     
           INSPECT WS-IDARTNR TALLYING NOLL-RAKNARE                             
                   FOR LEADING ZERO                                             
           ADD +1 TO NOLL-RAKNARE                                               
           UNSTRING WS-IDARTNR INTO                                             
                    UT-PART-ID WITH POINTER NOLL-RAKNARE                        
651208                                                                          
651714     MOVE WS-UT-TIAAMMDD  TO WS-TIAAMMDD                                  
                                   UT-DATUM-YYMMDD                              
651815     MOVE WS-AA1 TO WS-AA2                                                
651915     MOVE WS-MM1 TO WS-MM2                                                
652015     MOVE WS-DD1 TO WS-DD2                                                
652115     MOVE 20     TO WS-SS2                                                
652215     MOVE WS-DDMMAAAA     TO UT-ORD-DATE                                  
653006                                                                          
           IF UT-STAT = 'D'                                                     
555020        MOVE WS-HHMMSSTH TO WS-HHMMSSTH-D                                 
556020        ADD -1 TO WS-HH-D                                                 
556020        MOVE WS-HH-D    TO WS-TIME-HH-D                                   
580120        MOVE WS-MM-D    TO WS-TIME-MM-D                                   
580220        MOVE WS-SS-D    TO WS-TIME-SS-D                                   
580322        MOVE WS-TIME-D  TO UT-STAT-TIME                                   
           ELSE                                                                 
580322        MOVE WS-TIME  TO UT-STAT-TIME                                     
           END-IF                                                               
                                                                                
840006     PERFORM S01-SKRIV-W23686                                             
850023     .                                                                    
860001     EJECT                                                                
870001 C-SKAPA-SKRIV-BORTPOST SECTION.                                          
880000                                                                          
890000     MOVE FIL-WDR301 TO BORT-FIL-WDR301                                   
900006     PERFORM S02-SKRIV-W23687                                             
910000     .                                                                    
920000     EJECT                                                                
930000 Z-FINIT SECTION.                                                         
940000                                                                          
950006     CLOSE W23686                                                         
960006           W23687                                                         
970000                                                                          
980000     MOVE 'S' TO POSTSUM-OPKOD                                            
990000     CALL POSTSUM USING POSTSUM-PARM                                      
000000     .                                                                    
010000     EJECT                                                                
020006 S01-SKRIV-W23686 SECTION.                                                
030000                                                                          
040000     WRITE UT-POST FROM UT-AREA                                           
050000                                                                          
060000     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
070006     MOVE 'W23686 '  TO POSTSUM-FDNAMN                                    
080006     MOVE 'W23686D1' TO POSTSUM-DDNAMN2                                   
090000     CALL POSTSUM USING POSTSUM-PARM                                      
100000     .                                                                    
110000     SKIP3                                                                
120006 S02-SKRIV-W23687 SECTION.                                                
130000                                                                          
140000     WRITE BORT-POST FROM BORT-AREA                                       
150000                                                                          
160000     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
170006     MOVE 'W23687 '   TO POSTSUM-FDNAMN                                   
180001     MOVE 'W23686D2'  TO POSTSUM-DDNAMN2                                  
190000     CALL POSTSUM USING POSTSUM-PARM                                      
200000     .                                                                    
210000     EJECT                                                                
220001 S10-NOLLSTALL SECTION.                                                   
230001                                                                          
240005     MOVE SPACE TO UT-IDLEVNR                                             
250005                   UT-PART-ID                                             
251024                   UT-STAT                                                
260005                   UT-ORD-DATE                                            
270024     MOVE ZERO  TO UT-ORDERED                                             
280005                   UT-DELIVERED                                           
300023     .                                                                    
350001     EJECT                                                                
360000* --- IMS SEKTIONER ---                                                   
370000                                                                          
380000 IMS-GET-FILC-ROT SECTION.                                                
390001     STRING 'WDR301  (IDCPYTXT =' W-IDCPYTXT-X ')'                        
400000          DELIMITED BY SIZE INTO SSA1                                     
410000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
420001     CALL CBLTDLI USING GN WDR3-PCB DLI-IO-WDR301 SSA1                    
430001     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
440000     PERFORM IMS-STATUSKONTROLL                                           
450000     .                                                                    
460000     SKIP3                                                                
680000 IMS-STATUSKONTROLL SECTION.                                              
690000     SET STATUS-IX TO 1                                                   
700000     SEARCH GODK-STATUS                                                   
710000       AT END                                                             
720000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
730000           DELIMITED BY SIZE INTO FELTEXT                                 
740000         DISPLAY FELTEXT                                                  
750000         CALL FELLOG                                                      
760000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
770000         CONTINUE                                                         
780000     END-SEARCH                                                           
790000     .                                                                    
