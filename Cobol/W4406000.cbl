010000 ID DIVISION.                                                             
020000 PROGRAM-ID.                 W4406000.                                    
030000 AUTHOR.                     SVANTE BJÖRKBERG.                            
040000 DATE-WRITTEN.               JUNI 1988.                                   
050000     SKIP2                                                                
060000     REMARKS.                                                             
070000*                                                                         
130000*    FUNKTION.                                                            
140001*    *SB*                                                                 
150000*    LÄSER NER HELA WDA5 TILL 2 SEKV. FILER & EN EXTRAKTFIL               
160000*                                                                         
170000*    UTFILER: W44061, W44065                                              
180000*                                                                         
181000*    04-08-30 SM FLYTT AV LOCPREL-PRIS OM LOC-PRIS = ZERO                 
181100*    08-JUL-2021 SEND 2 FILES TO AZURE   PCCV363**                        
190000     EJECT                                                                
200000 ENVIRONMENT DIVISION.                                                    
210000 INPUT-OUTPUT SECTION.                                                    
220000 FILE-CONTROL.                                                            
230000     SKIP2                                                                
240000                                                                          
250000     SELECT W44061           ASSIGN TO      W44060D1.                     
251000     SELECT W44065           ASSIGN TO      W44060D2.                     
250000     SELECT W44094           ASSIGN TO      W44060D3.                     
251000     SELECT W44095           ASSIGN TO      W44060D4.                     
260000     EJECT                                                                
270000 DATA DIVISION.                                                           
280000 FILE SECTION.                                                            
290000     SKIP2                                                                
300000 FD  W44061                                                               
310000     LABEL RECORD STANDARD                                                
320000     RECORDING F                                                          
330000     BLOCK CONTAINS 0.                                                    
340000*01  W44061-POST -COPY W44060     -L                                      
350000 FD  W44065                                                               
351000     LABEL RECORD STANDARD                                                
352000     RECORDING F                                                          
353000     BLOCK CONTAINS 0.                                                    
354000*01  POST -COPY W440065   -PRE W44065-  -L.                               
      *                                                                         
300000 FD  W44094                                                               
310000     LABEL RECORD STANDARD                                                
320000     RECORDING F                                                          
330000     BLOCK CONTAINS 0.                                                    
340000*01  W44094-POST -COPY W4406X     -L                                      
      *                                                                         
300000 FD  W44095                                                               
310000     LABEL RECORD STANDARD                                                
320000     RECORDING F                                                          
330000     BLOCK CONTAINS 0.                                                    
340000*01  W44095-POST -COPY W4406X     -L                                      
355000     EJECT                                                                
360000 WORKING-STORAGE SECTION.                                                 
370000     SKIP2                                                                
       01 WS-DATE.                                                              
         03 WS-TODAYS-DATE               PIC 9(8)  VALUE ZERO.                  
         03 WS-YESTERDAY-AAAAMMDD        PIC 9(8)    VALUE ZERO.                
         03 FILLER REDEFINES WS-YESTERDAY-AAAAMMDD.                             
             05 WS-YESTERDAY-AA         PIC 9(2).                               
             05 WS-YESTERDAY-YYMMDD     PIC 9(6).                               
390000                                                                          
390100                                                                          
391000*    -- CHECKED BY WY2000                                                 
400000*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
410000                                                                          
420000 01  DYNAMISKA-SUBPROGRAM.                                                
430000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
440000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
450000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
         03  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.            
                                                                                
      *    -- SUBPROGRAM WZ20DAYS                                               
       01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
      *01 -COPY WZ20DAYS                                                        
                                                                                
460000     EJECT                                                                
470000*    ---- PARAMETRAR TILL POSTSUM                                         
480000                                                                          
490000*01  -COPY W0005      -PRE POSTSUM-.                                      
510000                                                                          
512000                                                                          
513200 01  FILLER                      PIC X(16)   VALUE                        
514000                                             'W-W44065-POST'.             
515000     SKIP3                                                                
516000*01  AREA -COPY W440065    -PRE UT1-.                                     
517000     EJECT                                                                
517100 01  FILLER                      PIC X(16)   VALUE                        
517200                                             'W-W44061-POST'.             
518000*01  AREA -COPY W44060     -PRE UT2-.                                     
519000     EJECT                                                                
519100 01  FILLER                      PIC X(16)   VALUE                        
519200                                             'W-W44060-POST'.             
518000*01  AREA -COPY W4406X     -PRE UT3-.                                     
519000     EJECT                                                                
519100 01  FILLER                      PIC X(16)   VALUE                        
519200                                             'W-W44094-POST'.             
518000*01  AREA -COPY W4406X     -PRE UT4-.                                     
519000     EJECT                                                                
519100 01  FILLER                      PIC X(16)   VALUE                        
519200                                             'W-W44095-POST'.             
520000*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
530000                                                                          
540000 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
550000                                                                          
560000*    ---- STATUSKOD FRÅN IMS                                              
570000                                                                          
580000 01  STATUS-WS                   PIC XX.                                  
590000     88  SEGMENT-FINNS                      VALUE '  '.                   
600000     88  SEGMENT-SLUT                       VALUE 'GB'.                   
610000     SKIP3                                                                
620000 01  GODK-STATUSKODER.                                                    
630000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
640000     SKIP3                                                                
650000 01  SSA1                        PIC X(40).                               
660000     EJECT                                                                
670000*01      -COPY W0003.                                                     
690000     EJECT                                                                
700000 01  FILLER                      PIC X(16)  VALUE                         
710000                                            'DLI-IO-AREA'.                
720000 01  DLI-IO-AREA.                                                         
740000*                                                                         
750000*  03  -COPY WDA501                                                       
770000     EJECT                                                                
780000 LINKAGE SECTION.                                                         
790000     SKIP2                                                                
800000*    -COPY W0008 -PRE WDA5-.                                              
820000    05  FILLER                   PIC XX.                                  
830000     EJECT                                                                
840000 PROCEDURE DIVISION  USING WDA5-PCB.                                      
850000     ENTRY 'DLITCBL' USING WDA5-PCB.                                      
860000     SKIP2                                                                
861000                                                                          
862000 STYR SECTION.                                                            
863000                                                                          
870000     PERFORM A-INIT                                                       
890000     PERFORM IMS-GET-WDA5                                                 
900000     PERFORM UNTIL SEGMENT-SLUT                                           
971000       PERFORM B-SKRIV-W44061                                             
972000       PERFORM C-SKRIV-W44065                                             
             PERFORM D-WRITE-BACKORDER-FILES                                    
980000       PERFORM IMS-GET-WDA5                                               
990000     END-PERFORM                                                          
000000                                                                          
010000     PERFORM Z-FINIT                                                      
020000     MOVE ZERO TO RETURN-CODE                                             
030000     GOBACK                                                               
031000     .                                                                    
040000     EJECT                                                                
050000 A-INIT SECTION.                                                          
060000     SKIP2                                                                
070000     OPEN OUTPUT W44061                                                   
071000                 W44065                                                   
                       W44094                                                   
                       W44095                                                   
           PERFORM AA-GET-YESTERDAY-DATE                                        
071200     MOVE 'W4406000'         TO POSTSUM-PROGNAMN                          
072000     .                                                                    
110000     EJECT                                                                
110100 AA-GET-YESTERDAY-DATE SECTION.                                           
                                                                                
           MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-TODAYS-DATE                   
           MOVE WS-TODAYS-DATE   TO DAYS-TIDATE1                                
           MOVE 'YYYYMMDD'       TO DAYS-KDDATFMT1                              
           MOVE 'YYYYMMDD'       TO DAYS-KDDATFMT2                              
           MOVE SPACE            TO DAYS-TIDATE2                                
                                    DAYS-IDCALEND                               
           MOVE -1               TO DAYS-KVDAYS                                 
                                                                                
           CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
                                                                                
           MOVE DAYS-TIDATE2(1:8) TO WS-YESTERDAY-AAAAMMDD                      
                                                                                
118200     .                                                                    
118300     EJECT                                                                
110100 B-SKRIV-W44061 SECTION.                                                  
111200     MOVE RAD-WDA501         TO UT2-RAD-W44060                            
111300                                                                          
117600     WRITE W44061-POST       FROM UT2-AREA                                
117700     MOVE 'W44061'           TO POSTSUM-FDNAMN                            
117800     MOVE 'W44060D1'         TO POSTSUM-DDNAMN2                           
117900     MOVE 'A501'             TO POSTSUM-TRANSTYP                          
118000                                                                          
118100     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
118200     .                                                                    
118300     EJECT                                                                
118400 C-SKRIV-W44065 SECTION.                                                  
118500                                                                          
118600     IF (RAD-KDTPOTYP > +0 AND NOT = +4) AND                              
118700        RAD-KDSTARAD = '3'  AND                                           
118800        RAD-DARODAT  = ZERO                                               
118900                                                                          
119000       MOVE  RAD-IDARTNR   TO    UT1-IDARTNR                              
119100       MOVE  RAD-IDDC      TO    UT1-IDDC                                 
119200       MOVE  RAD-KDORDKL   TO    UT1-KDORDKL                              
119300       MOVE  RAD-KVART     TO    UT1-KVART                                
119400                                                                          
119500       WRITE W44065-POST   FROM  UT1-AREA                                 
119600       MOVE 'W44060D2'     TO POSTSUM-DDNAMN2                             
119700       MOVE 'W44065  '     TO POSTSUM-FDNAMN                              
119800       MOVE 'TPO '         TO POSTSUM-TRANSTYP                            
119900                                                                          
120000       CALL  POSTSUM       USING POSTSUM-PARM                             
120100                                                                          
120200     END-IF                                                               
127500     .                                                                    
127600     EJECT                                                                
110100 D-WRITE-BACKORDER-FILES SECTION.                                         
                                                                                
           IF UT2-RAD-KDSTARAD = '4'                                            
              PERFORM DA-WRITE-W44094-FILE                                      
           ELSE                                                                 
               IF UT2-RAD-KDSTARAD = '1' OR                                     
                  UT2-RAD-KDSTARAD = '2' OR                                     
                  UT2-RAD-KDSTARAD = '3'                                        
                   PERFORM DB-WRITE-W44095-FILE                                 
               END-IF                                                           
           END-IF                                                               
118200     .                                                                    
118300     EJECT                                                                
110100 DA-WRITE-W44094-FILE SECTION.                                            
                                                                                
           IF UT2-RAD-TIAVBOKN = WS-YESTERDAY-YYMMDD                            
              PERFORM DAB-MOVE-VALUES-W4094                                     
              WRITE W44094-POST       FROM UT3-AREA                             
              MOVE 'W44094'           TO POSTSUM-FDNAMN                         
              MOVE 'W44060D3'         TO POSTSUM-DDNAMN2                        
              MOVE UT3-RAD-KDSTARAD   TO POSTSUM-TRANSTYP                       
                                                                                
              CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                     
           END-IF                                                               
                                                                                
118200     .                                                                    
118300     EJECT                                                                
110100 DAB-MOVE-VALUES-W4094  SECTION.                                          
                                                                                
           MOVE UT2-RAD-IDDISTR      TO UT3-RAD-IDDISTR                         
           MOVE UT2-RAD-IDKUNDNR     TO UT3-RAD-IDKUNDNR                        
           MOVE UT2-RAD-IDKUNDRF     TO UT3-RAD-IDKUNDRF                        
           MOVE UT2-RAD-IDARTNR      TO UT3-RAD-IDARTNR                         
           MOVE UT2-RAD-IDLOPNR      TO UT3-RAD-IDLOPNR                         
           MOVE UT2-RAD-BERADREF     TO UT3-RAD-BERADREF                        
           MOVE UT2-RAD-FLERS        TO UT3-RAD-FLERS                           
           MOVE UT2-RAD-IDANSK       TO UT3-RAD-IDANSK                          
           MOVE UT2-RAD-IDANALYS     TO UT3-RAD-IDANALYS                        
           MOVE UT2-RAD-IDKONTO      TO UT3-RAD-IDKONTO                         
           MOVE UT2-RAD-IDKST        TO UT3-RAD-IDKST                           
           MOVE UT2-RAD-IDKUNDRF-LEV TO UT3-RAD-IDKUNDRF-LEV                    
           MOVE UT2-RAD-IDDC         TO UT3-RAD-IDDC                            
           MOVE UT2-RAD-IDDC-RO      TO UT3-RAD-IDDC-RO                         
           MOVE UT2-RAD-KDDSP        TO UT3-RAD-KDDSP                           
           MOVE UT2-RAD-KDFAKTYP     TO UT3-RAD-KDFAKTYP                        
           MOVE UT2-RAD-KDFRAKT      TO UT3-RAD-KDFRAKT                         
           MOVE UT2-RAD-KDKVBRYT     TO UT3-RAD-KDKVBRYT                        
           MOVE UT2-RAD-KDOI         TO UT3-RAD-KDOI                            
           MOVE UT2-RAD-KDORDING     TO UT3-RAD-KDORDING                        
           MOVE UT2-RAD-KDORDKL      TO UT3-RAD-KDORDKL                         
           MOVE UT2-RAD-KDPRODSL     TO UT3-RAD-KDPRODSL                        
           MOVE UT2-RAD-KDRAPRIO     TO UT3-RAD-KDRAPRIO                        
           MOVE UT2-RAD-KDROO        TO UT3-RAD-KDROO                           
           MOVE UT2-RAD-KDSTARAD     TO UT3-RAD-KDSTARAD                        
           MOVE UT2-RAD-KDTPOTYP     TO UT3-RAD-KDTPOTYP                        
           MOVE UT2-RAD-KVART        TO UT3-RAD-KVART                           
           MOVE UT2-RAD-KDVRINFO     TO UT3-RAD-KDVRINFO                        
           MOVE UT2-RAD-KVRO         TO UT3-RAD-KVRO                            
           MOVE UT2-RAD-PRARTNTO     TO UT3-RAD-PRARTNTO                        
           MOVE UT2-RAD-REKSIFFR     TO UT3-RAD-REKSIFFR                        
           MOVE UT2-RAD-TIAVBOKN     TO UT3-RAD-TIAVBOKN                        
           MOVE UT2-RAD-TIREGDAT     TO UT3-RAD-TIREGDAT                        
           MOVE UT2-RAD-TIRES        TO UT3-RAD-TIRES                           
           MOVE UT2-RAD-DARODAT      TO UT3-RAD-DARODAT                         
           MOVE UT2-RAD-TITPO        TO UT3-RAD-TITPO                           
           MOVE UT2-RAD-KDPRTYP      TO UT3-RAD-KDPRTYP                         
           MOVE UT2-RAD-BEVOLREF     TO UT3-RAD-BEVOLREF                        
           MOVE UT2-RAD-FLINVEST     TO UT3-RAD-FLINVEST                        
           MOVE UT2-RAD-FLPRTILL     TO UT3-RAD-FLPRTILL                        
           MOVE UT2-RAD-FLTPOBEK     TO UT3-RAD-FLTPOBEK                        
           MOVE UT2-RAD-BEKUNDRF     TO UT3-RAD-BEKUNDRF                        
           MOVE UT2-RAD-IDKAMPRF     TO UT3-RAD-IDKAMPRF                        
           MOVE UT2-RAD-IDLEVNR      TO UT3-RAD-IDLEVNR                         
           MOVE UT2-RAD-IDSYSTEM     TO UT3-RAD-IDSYSTEM                        
           MOVE UT2-RAD-KVBEART-Q    TO UT3-RAD-KVBEART-Q                       
           MOVE UT2-RAD-TIREGTID     TO UT3-RAD-TIREGTID                        
           MOVE UT2-RAD-DASENBEK     TO UT3-RAD-DASENBEK                        
           MOVE UT2-RAD-IDPRQUES     TO UT3-RAD-IDPRQUES                        
           MOVE UT2-RAD-PRARTNTO-LOC TO UT3-RAD-PRARTNTO-LOC                    
           MOVE UT2-RAD-PRARTNTO-LOCPREL                                        
                                     TO UT3-RAD-PRARTNTO-LOCPREL                
           MOVE UT2-RAD-PRARTBTO-LOC TO UT3-RAD-PRARTBTO-LOC                    
           MOVE UT2-RAD-KDVALISO     TO UT3-RAD-KDVALISO                        
           MOVE UT2-RAD-KDVAT        TO UT3-RAD-KDVAT                           
           MOVE UT2-RAD-RERAB        TO UT3-RAD-RERAB                           
           MOVE UT2-RAD-KDRAB        TO UT3-RAD-KDRAB                           
           MOVE UT2-RAD-BEART-VIPS   TO UT3-RAD-BEART-VIPS                      
           MOVE UT2-RAD-KDORDTYP-LDC TO UT3-RAD-KDORDTYP-LDC                    
           MOVE UT2-RAD-IDKUNDRF-WIP TO UT3-RAD-IDKUNDRF-WIP                    
           MOVE UT2-RAD-TIREPDAT     TO UT3-RAD-TIREPDAT                        
           MOVE UT2-RAD-CLEARGROUP   TO UT3-RAD-CLEARGROUP                      
           MOVE UT2-RAD-PRAVCOST     TO UT3-RAD-PRAVCOST                        
           MOVE UT2-RAD-KDROPACK     TO UT3-RAD-KDROPACK                        
           MOVE UT2-RAD-IDARBREF     TO UT3-RAD-IDARBREF                        
           MOVE UT2-RAD-FILLER       TO UT3-RAD-FILLER                          
118200     .                                                                    
118300     EJECT                                                                
110100 DB-WRITE-W44095-FILE SECTION.                                            
                                                                                
           PERFORM DBB-MOVE-VALUES-W4095                                        
           WRITE W44095-POST       FROM UT4-AREA                                
           MOVE 'W44095'           TO POSTSUM-FDNAMN                            
           MOVE 'W44060D4'         TO POSTSUM-DDNAMN2                           
           MOVE UT4-RAD-KDSTARAD   TO POSTSUM-TRANSTYP                          
                                                                                
           CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
118200     .                                                                    
118300     EJECT                                                                
110100 DBB-MOVE-VALUES-W4095  SECTION.                                          
                                                                                
           MOVE UT2-RAD-IDDISTR      TO UT4-RAD-IDDISTR                         
           MOVE UT2-RAD-IDKUNDNR     TO UT4-RAD-IDKUNDNR                        
           MOVE UT2-RAD-IDKUNDRF     TO UT4-RAD-IDKUNDRF                        
           MOVE UT2-RAD-IDARTNR      TO UT4-RAD-IDARTNR                         
           MOVE UT2-RAD-IDLOPNR      TO UT4-RAD-IDLOPNR                         
           MOVE UT2-RAD-BERADREF     TO UT4-RAD-BERADREF                        
           MOVE UT2-RAD-FLERS        TO UT4-RAD-FLERS                           
           MOVE UT2-RAD-IDANSK       TO UT4-RAD-IDANSK                          
           MOVE UT2-RAD-IDANALYS     TO UT4-RAD-IDANALYS                        
           MOVE UT2-RAD-IDKONTO      TO UT4-RAD-IDKONTO                         
           MOVE UT2-RAD-IDKST        TO UT4-RAD-IDKST                           
           MOVE UT2-RAD-IDKUNDRF-LEV TO UT4-RAD-IDKUNDRF-LEV                    
           MOVE UT2-RAD-IDDC         TO UT4-RAD-IDDC                            
           MOVE UT2-RAD-IDDC-RO      TO UT4-RAD-IDDC-RO                         
           MOVE UT2-RAD-KDDSP        TO UT4-RAD-KDDSP                           
           MOVE UT2-RAD-KDFAKTYP     TO UT4-RAD-KDFAKTYP                        
           MOVE UT2-RAD-KDFRAKT      TO UT4-RAD-KDFRAKT                         
           MOVE UT2-RAD-KDKVBRYT     TO UT4-RAD-KDKVBRYT                        
           MOVE UT2-RAD-KDOI         TO UT4-RAD-KDOI                            
           MOVE UT2-RAD-KDORDING     TO UT4-RAD-KDORDING                        
           MOVE UT2-RAD-KDORDKL      TO UT4-RAD-KDORDKL                         
           MOVE UT2-RAD-KDPRODSL     TO UT4-RAD-KDPRODSL                        
           MOVE UT2-RAD-KDRAPRIO     TO UT4-RAD-KDRAPRIO                        
           MOVE UT2-RAD-KDROO        TO UT4-RAD-KDROO                           
           MOVE UT2-RAD-KDSTARAD     TO UT4-RAD-KDSTARAD                        
           MOVE UT2-RAD-KDTPOTYP     TO UT4-RAD-KDTPOTYP                        
           MOVE UT2-RAD-KVART        TO UT4-RAD-KVART                           
           MOVE UT2-RAD-KDVRINFO     TO UT4-RAD-KDVRINFO                        
           MOVE UT2-RAD-KVRO         TO UT4-RAD-KVRO                            
           MOVE UT2-RAD-PRARTNTO     TO UT4-RAD-PRARTNTO                        
           MOVE UT2-RAD-REKSIFFR     TO UT4-RAD-REKSIFFR                        
           MOVE UT2-RAD-TIAVBOKN     TO UT4-RAD-TIAVBOKN                        
           MOVE UT2-RAD-TIREGDAT     TO UT4-RAD-TIREGDAT                        
           MOVE UT2-RAD-TIRES        TO UT4-RAD-TIRES                           
           MOVE UT2-RAD-DARODAT      TO UT4-RAD-DARODAT                         
           MOVE UT2-RAD-TITPO        TO UT4-RAD-TITPO                           
           MOVE UT2-RAD-KDPRTYP      TO UT4-RAD-KDPRTYP                         
           MOVE UT2-RAD-BEVOLREF     TO UT4-RAD-BEVOLREF                        
           MOVE UT2-RAD-FLINVEST     TO UT4-RAD-FLINVEST                        
           MOVE UT2-RAD-FLPRTILL     TO UT4-RAD-FLPRTILL                        
           MOVE UT2-RAD-FLTPOBEK     TO UT4-RAD-FLTPOBEK                        
           MOVE UT2-RAD-BEKUNDRF     TO UT4-RAD-BEKUNDRF                        
           MOVE UT2-RAD-IDKAMPRF     TO UT4-RAD-IDKAMPRF                        
           MOVE UT2-RAD-IDLEVNR      TO UT4-RAD-IDLEVNR                         
           MOVE UT2-RAD-IDSYSTEM     TO UT4-RAD-IDSYSTEM                        
           MOVE UT2-RAD-KVBEART-Q    TO UT4-RAD-KVBEART-Q                       
           MOVE UT2-RAD-TIREGTID     TO UT4-RAD-TIREGTID                        
           MOVE UT2-RAD-DASENBEK     TO UT4-RAD-DASENBEK                        
           MOVE UT2-RAD-IDPRQUES     TO UT4-RAD-IDPRQUES                        
           MOVE UT2-RAD-PRARTNTO-LOC TO UT4-RAD-PRARTNTO-LOC                    
           MOVE UT2-RAD-PRARTNTO-LOCPREL                                        
                                     TO UT4-RAD-PRARTNTO-LOCPREL                
           MOVE UT2-RAD-PRARTBTO-LOC TO UT4-RAD-PRARTBTO-LOC                    
           MOVE UT2-RAD-KDVALISO     TO UT4-RAD-KDVALISO                        
           MOVE UT2-RAD-KDVAT        TO UT4-RAD-KDVAT                           
           MOVE UT2-RAD-RERAB        TO UT4-RAD-RERAB                           
           MOVE UT2-RAD-KDRAB        TO UT4-RAD-KDRAB                           
           MOVE UT2-RAD-BEART-VIPS   TO UT4-RAD-BEART-VIPS                      
           MOVE UT2-RAD-KDORDTYP-LDC TO UT4-RAD-KDORDTYP-LDC                    
           MOVE UT2-RAD-IDKUNDRF-WIP TO UT4-RAD-IDKUNDRF-WIP                    
           MOVE UT2-RAD-TIREPDAT     TO UT4-RAD-TIREPDAT                        
           MOVE UT2-RAD-CLEARGROUP   TO UT4-RAD-CLEARGROUP                      
           MOVE UT2-RAD-PRAVCOST     TO UT4-RAD-PRAVCOST                        
           MOVE UT2-RAD-KDROPACK     TO UT4-RAD-KDROPACK                        
           MOVE UT2-RAD-IDARBREF     TO UT4-RAD-IDARBREF                        
           MOVE UT2-RAD-FILLER       TO UT4-RAD-FILLER                          
118200     .                                                                    
118300     EJECT                                                                
128000 Z-FINIT SECTION.                                                         
130000     SKIP2                                                                
140000     CLOSE W44061                                                         
141000           W44065                                                         
141000           W44094                                                         
141000           W44095                                                         
150000     MOVE 'S' TO POSTSUM-OPKOD                                            
160000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
161000     .                                                                    
170000     EJECT                                                                
180000*    ---- IMS SEKTIONER                                                   
190000 IMS-GET-WDA5 SECTION.                                                    
200000                                                                          
210000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
220000     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-AREA                           
230000     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
240000     PERFORM IMS-STATUSKONTROLL.                                          
250000     SKIP3                                                                
260000 IMS-STATUSKONTROLL SECTION.                                              
270000                                                                          
280000     SET STATUS-IX TO 1                                                   
290000     SEARCH GODK-STATUS AT END CALL FELLOG                                
300000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
310000     CONTINUE                                                             
320000     END-SEARCH                                                           
330000     .                                                                    
