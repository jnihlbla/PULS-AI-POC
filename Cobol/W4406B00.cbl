010000 ID DIVISION.                                                             
020000 PROGRAM-ID.                 W4406B00.                                    
030000 AUTHOR.                     ARINDAM METIA.                               
040000 DATE-WRITTEN.               JULY 2023.                                   
050000     SKIP2                                                                
060000     REMARKS.                                                             
070000*                                                                         
130000*    FUNCTION.                                                            
150000*    CREATE 2 REPORT FORM WDA6 DATABASE WHERE TIKLAR = 0 & TIKALR         
160000*    = PREVEIOUS DAY. SEND TO AZURE DATA LAKE                             
180000*                                                                         
190000     EJECT                                                                
200000 ENVIRONMENT DIVISION.                                                    
210000 INPUT-OUTPUT SECTION.                                                    
220000 FILE-CONTROL.                                                            
230000     SKIP2                                                                
240000                                                                          
251000     SELECT W4406B1          ASSIGN TO      W4406BD1.                     
250000     SELECT W4406B2          ASSIGN TO      W4406BD2.                     
251000     EJECT                                                                
260000 DATA DIVISION.                                                           
270000 FILE SECTION.                                                            
280000     SKIP2                                                                
290000 FD  W4406B1                                                              
300000     LABEL RECORD STANDARD                                                
310000     RECORDING F                                                          
320000     BLOCK CONTAINS 0.                                                    
330000*01  POST -COPY W4406B    -PRE OPEN-  -L.                                 
340000 FD  W4406B2                                                              
350000     LABEL RECORD STANDARD                                                
351000     RECORDING F                                                          
352000     BLOCK CONTAINS 0.                                                    
353000*01  POST -COPY W4406B    -PRE PREV-  -L.                                 
354000*                                                                         
           EJECT                                                                
       WORKING-STORAGE SECTION.                                                 
           SKIP2                                                                
       01 WS-DATE.                                                              
         03 WS-TODAYS-DATE               PIC 9(8)  VALUE ZERO.                  
         03 WS-YESTERDAY-AAAAMMDD        PIC 9(8)  VALUE ZERO.                  
         03 FILLER REDEFINES WS-YESTERDAY-AAAAMMDD.                             
             05 WS-YESTERDAY-AA         PIC 9(2).                               
             05 WS-YESTERDAY-YYMMDD     PIC 9(6).                               
                                                                                
       01  DYNAMISKA-SUBPROGRAM.                                                
         03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
         03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
         03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
         03  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.            
                                                                                
       01 COUNTER                      PIC S9(5) COMP-3 VALUE +0.               
       01  ORDER-STATUS                PIC X(1).                                
         88  OPEN-ORDER                            VALUE 'X'.                   
         88  PREV-ORDER                            VALUE 'Y'.                   
         88  OLDER-ORDER                           VALUE 'Z'.                   
      *01 -COPY W4406B    -PRE OUT-                                             
353000*01  POST -COPY W4406B    -PRE INIT-                                      
                                                                                
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
520000*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
530000                                                                          
540000 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
550000                                                                          
560000*    ---- STATUSKOD FRÅN IMS                                              
570000                                                                          
                                                                                
       01  DISP-RECORD.                                                         
           03 DISP-VOR-IDDISTR          PIC 99999.                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDKUNDNR         PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDKUNDRF         PIC X(10).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIREGDAT-URSP    PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDARTNR          PIC 999999999.                          
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIREGTID-URSP    PIC 999999999.                          
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIREGDAT-AVV     PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIREGTID-AVV     PIC 999999999.                          
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDKUNDRF-LEV     PIC X(10).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIREGDAT-LEV     PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIREGTID-LEV     PIC 999999999.                          
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDROLL           PIC X(05).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDANSK           PIC 999.                                
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDLEVNR          PIC X(05).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-BERADREF         PIC X(10).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KVBEART-URSP     PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KVBEART          PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KVBEART-Q        PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KVPREAVB         PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDDC             PIC X(02).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDUSER           PIC X(08).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KDORDBEK         PIC 99.                                 
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KDPRTYP          PIC X(01).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KDVORATG         PIC X(01).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-PRARTNTO         PIC 9999999.99.                         
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TEVORMRK         PIC X(02).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIKLAR           PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIKLATID         PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIUPPDAT         PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TIUPPTID         PIC 999999999.                          
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-IDPRQUES         PIC 9999999.                            
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-PRARTNTO-LOC     PIC 9999999.99.                         
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-PRARTNTO-LOCPREL PIC 9999999.99.                         
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-PRARTBTO-LOC     PIC 9999999.99.                         
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KDVALISO         PIC X(03).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KDVAT            PIC X(02).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-RERAB            PIC 99.9.                               
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-KDRAB            PIC X(05).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-BEART-VIPS       PIC X(25).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TEVORMRK-SC      PIC X(02).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-FLVORFK          PIC X(01).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TEVORINT         PIC X(230).                             
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-FLLAEST-DEL      PIC X(01).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TEVOREXT         PIC X(310).                             
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-FLLAEST-SC       PIC X(01).                              
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TEVORSC          PIC X(310).                             
           03 FILLER                    PIC X(01) VALUE X'05'.                  
           03 DISP-VOR-TEVORDEL         PIC X(310).                             
                                                                                
580000 01  STATUS-WS                   PIC XX.                                  
590000     88  SEGMENT-FINNS                      VALUE '  '.                   
           88  SEGMENT-SAKNAS                     VALUE 'GB' 'GE'.              
610000     SKIP3                                                                
620000 01  GOOD-STATUS-CODE.                                                    
630000   03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
640000     SKIP3                                                                
650000 01  SSA1                        PIC X(40).                               
660000     EJECT                                                                
670000*01      -COPY W0003.                                                     
690000     EJECT                                                                
                                                                                
      *    ---  DLI INPUT-OUTPUT AREA                                           
                                                                                
       01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
       01  DLI-IO-WDA601.                                                       
      *    03  -COPY WDA601                                                     
           EJECT                                                                
       01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA611'.                      
       01  DLI-IO-WDA611.                                                       
      *    03  -COPY WDA611                                                     
           EJECT                                                                
       01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
       01  DLI-IO-WDA612.                                                       
      *    03  -COPY WDA612                                                     
           EJECT                                                                
       01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
       01  DLI-IO-WDA613.                                                       
      *    03  -COPY WDA613                                                     
           EJECT                                                                
       01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA614'.                      
       01  DLI-IO-WDA614.                                                       
      *    03  -COPY WDA614                                                     
           EJECT                                                                
                                                                                
780000 LINKAGE SECTION.                                                         
790000     SKIP2                                                                
800000*    -COPY W0008 -PRE WDA6-.                                              
820000    05  FILLER                   PIC XX.                                  
830000     EJECT                                                                
840000 PROCEDURE DIVISION  USING WDA6-PCB.                                      
850000     ENTRY 'DLITCBL' USING WDA6-PCB.                                      
860000     SKIP2                                                                
861000                                                                          
862000 STYR SECTION.                                                            
863000                                                                          
870000     PERFORM A-INIT                                                       
890000     PERFORM IMS-GN-WDA601                                                
900000     PERFORM UNTIL SEGMENT-SAKNAS                                         
971000       PERFORM B-CHECK-DATE                                               
             IF OPEN-ORDER OR PREV-ORDER                                        
972000          PERFORM C-MOVE-WDA601-DATA                                      
973000          PERFORM D-GET-REMAINING-SEGMENT                                 
974000          PERFORM E-WRITE-FILE                                            
             END-IF                                                             
980000       PERFORM IMS-GN-WDA601                                              
990000     END-PERFORM                                                          
000000                                                                          
010000     PERFORM Z-FINIT                                                      
020000     MOVE ZERO TO RETURN-CODE                                             
030000     GOBACK                                                               
031000     .                                                                    
040000     EJECT                                                                
050000 A-INIT SECTION.                                                          
060000     SKIP2                                                                
070000     OPEN OUTPUT W4406B1                                                  
071000                 W4406B2                                                  
           INITIALIZE INIT-POST                                                 
           MOVE INIT-POST        TO OUT-VOR-W4406B                              
           PERFORM AA-GET-YESTERDAY-DATE                                        
071200     MOVE 'W4406B00'       TO POSTSUM-PROGNAMN                            
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
110100 B-CHECK-DATE  SECTION.                                                   
                                                                                
           ADD +1 TO COUNTER                                                    
111200     IF VOR-TIKLAR = +0                                                   
              SET OPEN-ORDER     TO TRUE                                        
           ELSE                                                                 
111300        IF VOR-TIKLAR = WS-YESTERDAY-YYMMDD                               
                 SET PREV-ORDER  TO TRUE                                        
              ELSE                                                              
                 SET OLDER-ORDER TO TRUE                                        
              END-IF                                                            
           END-IF                                                               
118200     .                                                                    
118300     EJECT                                                                
118400 C-MOVE-WDA601-DATA SECTION.                                              
118500                                                                          
           MOVE INIT-POST               TO OUT-VOR-W4406B                       
           MOVE VOR-IDDISTR             TO OUT-VOR-IDDISTR                      
           MOVE VOR-IDKUNDNR            TO OUT-VOR-IDKUNDNR                     
           MOVE VOR-IDKUNDRF            TO OUT-VOR-IDKUNDRF                     
           MOVE VOR-TIREGDAT-URSP       TO OUT-VOR-TIREGDAT-URSP                
           MOVE VOR-IDARTNR             TO OUT-VOR-IDARTNR                      
           MOVE VOR-TIREGTID-URSP       TO OUT-VOR-TIREGTID-URSP                
           MOVE VOR-TIREGDAT-AVV        TO OUT-VOR-TIREGDAT-AVV                 
           MOVE VOR-TIREGTID-AVV        TO OUT-VOR-TIREGTID-AVV                 
           MOVE VOR-IDKUNDRF-LEV        TO OUT-VOR-IDKUNDRF-LEV                 
           MOVE VOR-TIREGDAT-LEV        TO OUT-VOR-TIREGDAT-LEV                 
           MOVE VOR-TIREGTID-LEV        TO OUT-VOR-TIREGTID-LEV                 
           MOVE VOR-IDROLL              TO OUT-VOR-IDROLL                       
           MOVE VOR-IDANSK              TO OUT-VOR-IDANSK                       
           MOVE VOR-IDLEVNR             TO OUT-VOR-IDLEVNR                      
           MOVE VOR-BERADREF            TO OUT-VOR-BERADREF                     
           MOVE VOR-KVBEART-URSP        TO OUT-VOR-KVBEART-URSP                 
           MOVE VOR-KVBEART             TO OUT-VOR-KVBEART                      
           MOVE VOR-KVBEART-Q           TO OUT-VOR-KVBEART-Q                    
           MOVE VOR-KVPREAVB            TO OUT-VOR-KVPREAVB                     
           MOVE VOR-IDDC                TO OUT-VOR-IDDC                         
           MOVE VOR-IDUSER              TO OUT-VOR-IDUSER                       
           MOVE VOR-KDORDBEK            TO OUT-VOR-KDORDBEK                     
           MOVE VOR-KDPRTYP             TO OUT-VOR-KDPRTYP                      
           MOVE VOR-KDVORATG            TO OUT-VOR-KDVORATG                     
           MOVE VOR-PRARTNTO            TO OUT-VOR-PRARTNTO                     
           MOVE VOR-TEVORMRK            TO OUT-VOR-TEVORMRK                     
           MOVE VOR-TIKLAR              TO OUT-VOR-TIKLAR                       
           MOVE VOR-TIKLATID            TO OUT-VOR-TIKLATID                     
           MOVE VOR-TIUPPDAT            TO OUT-VOR-TIUPPDAT                     
           MOVE VOR-TIUPPTID            TO OUT-VOR-TIUPPTID                     
           MOVE VOR-IDPRQUES            TO OUT-VOR-IDPRQUES                     
           MOVE VOR-PRARTNTO-LOC        TO OUT-VOR-PRARTNTO-LOC                 
           MOVE VOR-PRARTNTO-LOCPREL    TO OUT-VOR-PRARTNTO-LOCPREL             
           MOVE VOR-PRARTBTO-LOC        TO OUT-VOR-PRARTBTO-LOC                 
           MOVE VOR-KDVALISO            TO OUT-VOR-KDVALISO                     
           MOVE VOR-KDVAT               TO OUT-VOR-KDVAT                        
           MOVE VOR-RERAB               TO OUT-VOR-RERAB                        
           MOVE VOR-KDRAB               TO OUT-VOR-KDRAB                        
           MOVE VOR-BEART-VIPS          TO OUT-VOR-BEART-VIPS                   
           MOVE VOR-TEVORMRK-SC         TO OUT-VOR-TEVORMRK-SC                  
           MOVE VOR-FLVORFK             TO OUT-VOR-FLVORFK                      
127500     .                                                                    
127600     EJECT                                                                
118400 D-GET-REMAINING-SEGMENT SECTION.                                         
118500                                                                          
           PERFORM IMS-GNP-WDA611                                               
           IF SEGMENT-FINNS                                                     
              MOVE VTI-TEVORINT         TO OUT-VOR-TEVORINT                     
           END-IF                                                               
                                                                                
           PERFORM IMS-GNP-WDA612                                               
           IF SEGMENT-FINNS                                                     
              MOVE VTE-FLLAEST          TO OUT-VOR-FLLAEST-DEL                  
              MOVE VTE-TEVOREXT         TO OUT-VOR-TEVOREXT                     
           END-IF                                                               
                                                                                
           PERFORM IMS-GNP-WDA613                                               
           IF SEGMENT-FINNS                                                     
              MOVE VTS-FLLAEST          TO OUT-VOR-FLLAEST-SC                   
              MOVE VTS-TEVORSC          TO OUT-VOR-TEVORSC                      
           END-IF                                                               
                                                                                
           PERFORM IMS-GNP-WDA614                                               
           IF SEGMENT-FINNS                                                     
              MOVE VTD-TEVORDEL         TO OUT-VOR-TEVORDEL                     
           END-IF                                                               
           .                                                                    
127600     EJECT                                                                
                                                                                
110100 E-WRITE-FILE SECTION.                                                    
                                                                                
           PERFORM F-DISP-FORMAT                                                
           IF OPEN-ORDER                                                        
              PERFORM EA-WRITE-W4406B1-FILE                                     
           ELSE                                                                 
               IF PREV-ORDER                                                    
                   PERFORM EB-WRITE-W4406B2-FILE                                
               END-IF                                                           
           END-IF                                                               
118200     .                                                                    
118300     EJECT                                                                
110100 F-DISP-FORMAT SECTION.                                                   
                                                                                
           MOVE OUT-VOR-IDDISTR          TO DISP-VOR-IDDISTR                    
           MOVE OUT-VOR-IDKUNDNR         TO DISP-VOR-IDKUNDNR                   
           MOVE OUT-VOR-IDKUNDRF         TO DISP-VOR-IDKUNDRF                   
           MOVE OUT-VOR-TIREGDAT-URSP    TO DISP-VOR-TIREGDAT-URSP              
           MOVE OUT-VOR-IDARTNR          TO DISP-VOR-IDARTNR                    
           MOVE OUT-VOR-TIREGTID-URSP    TO DISP-VOR-TIREGTID-URSP              
           MOVE OUT-VOR-TIREGDAT-AVV     TO DISP-VOR-TIREGDAT-AVV               
           MOVE OUT-VOR-TIREGTID-AVV     TO DISP-VOR-TIREGTID-AVV               
           MOVE OUT-VOR-IDKUNDRF-LEV     TO DISP-VOR-IDKUNDRF-LEV               
           MOVE OUT-VOR-TIREGDAT-LEV     TO DISP-VOR-TIREGDAT-LEV               
           MOVE OUT-VOR-TIREGTID-LEV     TO DISP-VOR-TIREGTID-LEV               
           MOVE OUT-VOR-IDROLL           TO DISP-VOR-IDROLL                     
           MOVE OUT-VOR-IDANSK           TO DISP-VOR-IDANSK                     
           MOVE OUT-VOR-IDLEVNR          TO DISP-VOR-IDLEVNR                    
           MOVE OUT-VOR-BERADREF         TO DISP-VOR-BERADREF                   
           MOVE OUT-VOR-KVBEART-URSP     TO DISP-VOR-KVBEART-URSP               
           MOVE OUT-VOR-KVBEART          TO DISP-VOR-KVBEART                    
           MOVE OUT-VOR-KVBEART-Q        TO DISP-VOR-KVBEART-Q                  
           MOVE OUT-VOR-KVPREAVB         TO DISP-VOR-KVPREAVB                   
           MOVE OUT-VOR-IDDC             TO DISP-VOR-IDDC                       
           MOVE OUT-VOR-IDUSER           TO DISP-VOR-IDUSER                     
           MOVE OUT-VOR-KDORDBEK         TO DISP-VOR-KDORDBEK                   
           MOVE OUT-VOR-KDPRTYP          TO DISP-VOR-KDPRTYP                    
           MOVE OUT-VOR-KDVORATG         TO DISP-VOR-KDVORATG                   
           MOVE OUT-VOR-PRARTNTO         TO DISP-VOR-PRARTNTO                   
           MOVE OUT-VOR-TEVORMRK         TO DISP-VOR-TEVORMRK                   
           MOVE OUT-VOR-TIKLAR           TO DISP-VOR-TIKLAR                     
           MOVE OUT-VOR-TIKLATID         TO DISP-VOR-TIKLATID                   
           MOVE OUT-VOR-TIUPPDAT         TO DISP-VOR-TIUPPDAT                   
           MOVE OUT-VOR-TIUPPTID         TO DISP-VOR-TIUPPTID                   
           MOVE OUT-VOR-IDPRQUES         TO DISP-VOR-IDPRQUES                   
           MOVE OUT-VOR-PRARTNTO-LOC     TO DISP-VOR-PRARTNTO-LOC               
           MOVE OUT-VOR-PRARTNTO-LOCPREL TO DISP-VOR-PRARTNTO-LOCPREL           
           MOVE OUT-VOR-PRARTBTO-LOC     TO DISP-VOR-PRARTBTO-LOC               
           MOVE OUT-VOR-KDVALISO         TO DISP-VOR-KDVALISO                   
           MOVE OUT-VOR-KDVAT            TO DISP-VOR-KDVAT                      
           MOVE OUT-VOR-RERAB            TO DISP-VOR-RERAB                      
           MOVE OUT-VOR-KDRAB            TO DISP-VOR-KDRAB                      
           MOVE OUT-VOR-BEART-VIPS       TO DISP-VOR-BEART-VIPS                 
           MOVE OUT-VOR-TEVORMRK-SC      TO DISP-VOR-TEVORMRK-SC                
           MOVE OUT-VOR-FLVORFK          TO DISP-VOR-FLVORFK                    
           MOVE OUT-VOR-TEVORINT         TO DISP-VOR-TEVORINT                   
           MOVE OUT-VOR-FLLAEST-DEL      TO DISP-VOR-FLLAEST-DEL                
           MOVE OUT-VOR-TEVOREXT         TO DISP-VOR-TEVOREXT                   
           MOVE OUT-VOR-FLLAEST-SC       TO DISP-VOR-FLLAEST-SC                 
           MOVE OUT-VOR-TEVORSC          TO DISP-VOR-TEVORSC                    
           MOVE OUT-VOR-TEVORDEL         TO DISP-VOR-TEVORDEL                   
           .                                                                    
           EJECT                                                                
                                                                                
110100 EA-WRITE-W4406B1-FILE SECTION.                                           
                                                                                
           WRITE OPEN-POST            FROM DISP-RECORD                          
           MOVE 'W4406B1' TO POSTSUM-FDNAMN                                     
           MOVE 'W4406BD1' TO POSTSUM-DDNAMN2                                   
           CALL POSTSUM USING POSTSUM-PARM                                      
118200     .                                                                    
118300     EJECT                                                                
110100 EB-WRITE-W4406B2-FILE SECTION.                                           
                                                                                
           WRITE PREV-POST            FROM DISP-RECORD                          
           MOVE 'W4406B2' TO POSTSUM-FDNAMN                                     
           MOVE 'W4406BD2' TO POSTSUM-DDNAMN2                                   
           CALL POSTSUM USING POSTSUM-PARM                                      
118200     .                                                                    
118300     EJECT                                                                
128000 Z-FINIT SECTION.                                                         
130000     SKIP2                                                                
140000     CLOSE W4406B1                                                        
141000           W4406B2                                                        
           MOVE 'S' TO POSTSUM-OPKOD                                            
           CALL POSTSUM USING POSTSUM-PARM                                      
161000     .                                                                    
170000     EJECT                                                                
180000*    ---- IMS SEKTIONER                                                   
190000 IMS-GN-WDA601 SECTION.                                                   
           MOVE 'WDA601 ' TO SSA1                                               
210000     MOVE '  GBGE' TO GOOD-STATUS-CODE                                    
220000     CALL CBLTDLI USING GN WDA6-PCB DLI-IO-WDA601 SSA1                    
230000     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
240000     PERFORM IMS-STATUS-CHECK                                             
           .                                                                    
250000     SKIP3                                                                
       IMS-GNP-WDA611 SECTION.                                                  
           MOVE 'WDA611 ' TO SSA1                                               
           MOVE '  GE' TO GOOD-STATUS-CODE                                      
           CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA611 SSA1                   
           MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUS-CHECK                                             
           .                                                                    
           SKIP3                                                                
       IMS-GNP-WDA612 SECTION.                                                  
           MOVE 'WDA612 ' TO SSA1                                               
           MOVE '  GE' TO GOOD-STATUS-CODE                                      
           CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA612 SSA1                   
           MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUS-CHECK                                             
           .                                                                    
           SKIP3                                                                
       IMS-GNP-WDA613 SECTION.                                                  
           MOVE 'WDA613 ' TO SSA1                                               
           MOVE '  GE' TO GOOD-STATUS-CODE                                      
           CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA613 SSA1                   
           MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUS-CHECK                                             
           .                                                                    
           SKIP3                                                                
       IMS-GNP-WDA614 SECTION.                                                  
           MOVE 'WDA614 ' TO SSA1                                               
           MOVE '  GE' TO GOOD-STATUS-CODE                                      
           CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA614 SSA1                   
           MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUS-CHECK                                             
           .                                                                    
           SKIP3                                                                
278300 IMS-STATUS-CHECK   SECTION.                                              
278400                                                                          
278500     SET STATUS-IX TO 1                                                   
278600     SEARCH GOOD-STATUS                                                   
278700       AT END                                                             
278800         CALL FELLOG                                                      
278900     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
279000       CONTINUE                                                           
279100     END-SEARCH                                                           
280000     .                                                                    
