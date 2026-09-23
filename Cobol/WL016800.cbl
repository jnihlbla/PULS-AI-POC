000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     WL016800.                                                
000301 AUTHOR.         HÅKAN BOHLIN.                                            
000401 DATE-WRITTEN.   19/11/07.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    NAMN:CARPARTS.LDC.COREARRIVALREPORT                                  
000801*    WEB-LDC: WL016800 PROGRAM IS A REPLICA OF W3017100 PROGRAM           
000901*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001001*                                                                         
001101*    FUNKTION:                                                            
001201*        LOCATION ENQUIRY LDC                                             
001301*        PROGRAM READS FOLLOWING DATABASE WDM6                            
001401*                                         WDM6D                           
001501*                                         WDB6                            
001601*                                 UPDATES WDR2                            
001701*    INDATA.                                                              
001801*        TRANSAKTION: WL0168T                                             
001901*        REQUEST:     WZ01REQU                                            
002001*                     WL0168I1                                            
002101*                                                                         
002201*    UTDATA.                                                              
002301*        RESPONSE:    WZ01RESP                                            
002401*                     WL0168O1                                            
002501*                                                                         
002601                                                                          
002701 ENVIRONMENT DIVISION.                                                    
002801                                                                          
002901                                                                          
003001 DATA DIVISION.                                                           
003101     EJECT                                                                
003201 WORKING-STORAGE SECTION.                                                 
003301 77  IDPGM                       PIC X(08)   VALUE 'WL016800'.            
003401 77  WS-ADRESS                   PIC X(50)   VALUE                        
003501     'CARPARTS.LDC.COREARRIVALREPORT'.                                    
003601*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003701 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003801 77  CURRENT-IMS-SECTION         PIC X(25) VALUE SPACE.                   
003901 77  CURRENT-SECTION             PIC X(25) VALUE SPACE.                   
004001 77  KDRC-DISPLAY                PIC Z(5).                                
004101 77  WS-RESP-AREA                PIC S9(6) VALUE ZERO COMP-3.             
004201                                                                          
004301 77  YES                         PIC X          VALUE 'J'.                
004401 77  NOO                         PIC X          VALUE 'N'.                
004501 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004601 77  MAX-INDX                    PIC S9(4)  VALUE +3000 COMP SYNC.        
004701 77  WS-KVRADER                  PIC  9(5)      VALUE ZERO.               
004801 77  WS-VKORDBTO                 PIC  9(6)V9(1) VALUE ZERO.               
004901 77  WS-VLORDBTO                 PIC  9(4)V9(3) VALUE ZERO.               
005001                                                                          
006001 01  WS-WORK-FIELDS.                                                      
006101   03  WS-IDDISTR-KEY            PIC S9(5)   VALUE ZERO.                  
006201   03  WS-IDKUNDNR-KEY           PIC S9(7)   VALUE ZERO.                  
006301   03  WS-IDBYTRAP-KEY           PIC S9(7)   VALUE ZERO.                  
006401   03  WS-KDBYTSTA-KEY           PIC X(1)    VALUE SPACE.                 
006501   03  WS-IDDC-KEY               PIC X(2)    VALUE SPACE.                 
006601   03  WS-IDDC-REC-KEY           PIC X(2)    VALUE SPACE.                 
006701                                                                          
006801 01  DAGENS-DATUM                PIC 9(8).                                
006901 01  FILLER REDEFINES DAGENS-DATUM.                                       
007001     03 DAGENS-DATUM-AA          PIC 9(2).                                
008001     03 DAGENS-DATUM-AAMMDD      PIC 9(6).                                
008101                                                                          
008201 01  DAGENS-TID                  PIC 9(8).                                
008301                                                                          
008401 01  WS-STYR-LAS.                                                         
008501   03  WS-DISTR-STYR             PIC X(1)    VALUE SPACE.                 
008601   03  WS-KUND-STYR              PIC X(1)    VALUE SPACE.                 
008701   03  WS-RAPP-STYR              PIC X(1)    VALUE SPACE.                 
008801   03  WS-STATUS-STYR            PIC X(1)    VALUE SPACE.                 
008901                                                                          
009001 77  KEYS-SW                     PIC X       VALUE 'J'.                   
010001     88  KEYS-OK                             VALUE 'J'.                   
010101                                                                          
010201 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010301     88  INDATA-OK                           VALUE 'J'.                   
010401                                                                          
010501 77  SHIPDOC-SW                  PIC X       VALUE 'J'.                   
010601     88  SHIPDOC-OK                          VALUE 'J'.                   
010701                                                                          
010801 77  WDGX3148-SW                 PIC X       VALUE 'J'.                   
010901     88  WDGX3148-OK                         VALUE 'J'.                   
011001     88  WDGX3148-NOT-EXIST                  VALUE 'N'.                   
011101                                                                          
011201 77  FORSTA-SW                   PIC X       VALUE 'J'.                   
011301     88  FORSTA-OK                           VALUE 'J'.                   
011401                                                                          
011501 77  CHANGE-WEIGHT-SW            PIC X       VALUE 'N'.                   
011601     88  CHANGE-WEIGHT-OK                    VALUE 'J'.                   
011701     88  CHANGE-NO-WEIGHT                    VALUE 'N'.                   
011801                                                                          
011900 77  CHANGE-VOLUME-SW            PIC X       VALUE 'N'.                   
012000     88  CHANGE-VOLUME-OK                    VALUE 'J'.                   
013000     88  CHANGE-NO-VOLUME                    VALUE 'N'.                   
013101                                                                          
013201 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013301 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013401 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013501     SKIP3                                                                
013601 01  MESSAGE-CODES.                                                       
013701     03  SYSTEM-ERROR            PIC X(3)  VALUE '099'.                   
013801     EJECT                                                                
013901                                                                          
014001*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
015001 01  GENERAL-SUBPROGRAMS.                                                 
015101     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015201     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015301     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
015401     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015501     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
015601                                                                          
015701 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
015801*01  -COPY WZ01SUB                                                        
015901                                                                          
016001 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
016101*01  -COPY WDECAREA                                                       
016201     EJECT                                                                
016301                                                                          
016401*01  -COPY WWDCKONS                                                       
016501                                                                          
016601 01  FILLER                      PIC X(8)  VALUE 'SOP     '.              
016701*01  -COPY WMSGSOP                                                        
016801     EJECT                                                                
016901                                                                          
017001 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
018001 01  REQU-AREA.                                                           
018101*    03  -COPY WZ01REQU                                                   
018201*    03  -COPY WL0168I1                                                   
018301     EJECT                                                                
018401 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
018501 01  RESP-AREA.                                                           
018601*    03  -COPY WZ01RESP                                                   
018701*    03  -COPY WL0168O1                                                   
018801                                                                          
018901     EJECT                                                                
019001 01  NYCKLAR-TILL-DLI.                                                    
020001   03  W-IDBYTRAP-X.                                                      
021001     05 W-IDBYTRAP           PIC S9(7)   VALUE ZERO COMP-3.               
021101   03  W-IDKUNDNR-X.                                                      
021201     05 W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.               
021301                                                                          
021401   03  W-WDM6D1KY-MIN-X.                                                  
021501     05  W-IDDC-D1           PIC X(2)    VALUE SPACE.                     
021601     05  W-IDDISTR-D1        PIC S9(5)   VALUE ZERO COMP-3.               
021701     05  W-KDBYTSTA-D1       PIC X(1)    VALUE SPACE.                     
021801     05  FILLER              PIC X(12)   VALUE LOW-VALUE.                 
021901                                                                          
022001   03  W-WDM6D1KY-MAX-X.                                                  
023001     05  W-IDDC-D1-MAX       PIC X(2)    VALUE SPACE.                     
024001     05  W-IDDISTR-D1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
025001     05  W-KDBYTSTA-D1-MAX   PIC X(1)    VALUE '9'.                       
026001     05  FILLER              PIC X(12)   VALUE HIGH-VALUE.                
027001                                                                          
027101   03  W-WDM601KY-X.                                                      
027201     05  W-IDDISTR-UNIK      PIC S9(5)   VALUE ZERO COMP-3.               
027301     05  W-IDBYTRAP-UNIK     PIC S9(7)   VALUE ZERO COMP-3.               
027401                                                                          
027501   03  W-IDDC-B6-X.                                                       
027601       05 W-IDDC-B6          PIC X(2).                                    
027701                                                                          
027801   03  W-WDGXKEY-3147-X.                                                  
027901       05  W-3147-IDHTYP     PIC X(4)    VALUE '3147'.                    
028001       05  FILLER            PIC X(26)   VALUE LOW-VALUE.                 
028101   03  W-3148KY-X.                                                        
028201       05  W-3148-IDUSER     PIC X(8)    VALUE SPACE.                     
028301       05  W-3148-IDDC       PIC X(2)    VALUE SPACE.                     
028302       05  W-3148-IDDC-REC   PIC X(2)    VALUE SPACE.                     
028401   03  W-3150KY-X.                                                        
028501       05  W-3150-IDDISTR    PIC S9(5)   VALUE ZERO COMP-3.               
028601       05  W-3150-IDBYTRAP   PIC S9(7)   VALUE ZERO COMP-3.               
028701                                                                          
028801*    --- STATUS-KOD FRÅN IMS                                              
028901 01  STATUS-WS                   PIC XX.                                  
029001     88  SEGMENT-FINNS                       VALUE '  '.                  
029101     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029201     88  BASEN-SLUT                          VALUE 'GB'.                  
029301     SKIP2                                                                
029401 01  GODK-STATUSKODER.                                                    
029501     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029601     SKIP3                                                                
029701 01  SSA1                        PIC X(156).                              
029801 01  SSA2                        PIC X(64).                               
029901 01  SSA3                        PIC X(64).                               
030001                                                                          
030101*    --- IMS FUNKTIONSKODER                                               
030201*01  -COPY W0003                                                          
030301     EJECT                                                                
030401*    ---  DLI INPUT-OUTPUT AREA                                           
030501 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
030601                                                                          
030701 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDM601'.           
030801 01  DLI-IO-WDM601.                                                       
030901*  03  -COPY WDM601                                                       
031001     EJECT                                                                
031101                                                                          
031201 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDM611'.           
031301                                                                          
031401 01  DLI-IO-WDM611.                                                       
031501*  03  -COPY WDM611                                                       
031601     EJECT                                                                
031701                                                                          
031801 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDM6D1'.           
031901                                                                          
032001 01  DLI-IO-WDM6D1.                                                       
033001*  03  -COPY WDM6D1                                                       
033101                                                                          
033201 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB601'.           
033301 01   DLI-IO-AREA-B601.                                                   
033401*     03  -COPY WDB601                                                    
033501                                                                          
033601     EJECT                                                                
033701 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDGX3148'.         
033801 01  DLI-IO-WDGX3148.                                                     
033901*    03  -COPY WDGX3148                                                   
034001     EJECT                                                                
035001 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDGX3150'.         
035101 01  DLI-IO-WDGX3150.                                                     
035201*    03  -COPY WDGX3150                                                   
035301     EJECT                                                                
035401                                                                          
035501 LINKAGE SECTION.                                                         
035601*01  -COPY W0009  -PRE MSG-                                               
035701     EJECT                                                                
035801*01  -COPY W0009  -PRE ALT-                                               
035901     EJECT                                                                
036001                                                                          
036101*01  -COPY W0008  -PRE WDM6-                                              
036201     05  FILLER                  PIC X.                                   
036301*01  -COPY W0008  -PRE WDM6D-                                             
036401     05  FILLER                  PIC X.                                   
036501*01  -COPY W0008  -PRE WDB6-                                              
036601     05  FILLER                  PIC X.                                   
036701*01  -COPY W0008  -PRE 3147-                                              
036801     05  FILLER                  PIC X.                                   
036901     EJECT                                                                
037001                                                                          
038001 PROCEDURE DIVISION  USING                                                
039001                           MSG-PCB ALT-PCB                                
039101                           WDM6-PCB WDM6D-PCB                             
039201                           WDB6-PCB 3147-PCB.                             
039301 MAIN SECTION.                                                            
039401     ENTRY 'DLITCBL' USING                                                
039501                           MSG-PCB ALT-PCB                                
039601                           WDM6-PCB WDM6D-PCB                             
039701                           WDB6-PCB 3147-PCB.                             
039801                                                                          
039901                                                                          
040001*------------------------                                                 
041001     MOVE 'STYR SECTION'     TO CURRENT-SECTION                           
041101     PERFORM S01-FETCH-REQUDATA                                           
041201     IF SUB-KDRC = 0                                                      
041301       PERFORM A-INIT                                                     
041401       PERFORM B-CHECK-KEYS                                               
041501       IF KEYS-OK                                                         
041601         IF REQU-KDPGMACT = 'S' OR 'B'                                    
041701           PERFORM F-READ-SHOW-INFO                                       
041801         END-IF                                                           
041901         IF REQU-KDPGMACT = 'V'                                           
042001           PERFORM C-UPDATE-WDGX3150                                      
043001           IF FORSTA-OK AND WDGX3148-NOT-EXIST                            
043101              MOVE ZERO TO RESP-KVRADER                                   
043201           ELSE                                                           
043301              PERFORM D-VIEW-SHIPDOC                                      
043401           END-IF                                                         
043501         END-IF                                                           
043601         IF REQU-KDPGMACT = 'E'                                           
043701           PERFORM G-CHECK-INPUT                                          
043801           IF INDATA-OK                                                   
043901             PERFORM H-UPDATE                                             
044001             IF REQU-FLKLAR = YES                                         
044101               CONTINUE                                                   
044201             ELSE                                                         
044301               PERFORM D-VIEW-SHIPDOC                                     
044401             END-IF                                                       
044501           ELSE                                                           
044601             PERFORM D-VIEW-SHIPDOC                                       
044701           END-IF                                                         
044801         END-IF                                                           
044901       END-IF                                                             
045001       PERFORM S02-RETURN-ANSWER                                          
045101     END-IF                                                               
045201                                                                          
045301     PERFORM Z-FINIT                                                      
045401     MOVE ZERO TO RETURN-CODE                                             
045501     GOBACK                                                               
045601     .                                                                    
045701     EJECT                                                                
045801                                                                          
045901 A-INIT SECTION.                                                          
046001     MOVE 'A-INIT'    TO CURRENT-SECTION                                  
046101     MOVE ALL '+' TO RESP-AREA                                            
046201     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
046301                     RESP-IDMSG-INFO                                      
046401                     RESP-IDELMT-ERROR                                    
046501     IF REQU-KDPGMACT = 'S' OR 'B' OR 'V' OR 'E'                          
046601       MOVE ALL SPACE TO RESP-AREA                                        
046701     END-IF                                                               
046801                                                                          
046901     MOVE ZERO    TO RESP-KVRADER                                         
047001     MOVE '001'   TO RESP-IDMSGVER                                        
047101     ACCEPT DAGENS-DATUM FROM DATE                                        
047201     ACCEPT DAGENS-TID  FROM TIME                                         
047301     .                                                                    
047401     EJECT                                                                
047501                                                                          
047601 B-CHECK-KEYS SECTION.                                                    
047701                                                                          
047801     MOVE 'B-CHECK-KEYS'     TO CURRENT-SECTION                           
047901     MOVE YES TO KEYS-SW                                                  
048001*** ONLY REPORTS IN STATUS 2 ARE VALID TO SEE FOR LDC AND SDC.            
048101     MOVE 2 TO WS-KDBYTSTA-KEY                                            
048201                                                                          
048301***  CONTROL OF KDPGMACT                                                  
048401     IF REQU-KDPGMACT = 'S' OR 'B' OR 'V' OR 'E'                          
048501        CONTINUE                                                          
048601     ELSE                                                                 
048701        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
048801        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
048901        MOVE NOO TO KEYS-SW                                               
049001     END-IF                                                               
049101                                                                          
049201***  CONTROL OF DISTRICT                                                  
049301     IF KEYS-OK                                                           
049401       IF REQU-IDDISTR-KEY = ALL '+'                                      
049501         MOVE NOO TO KEYS-SW                                              
049601         MOVE 'IDDISTR' TO RESP-IDELMT-ERROR                              
049701         MOVE '026'           TO RESP-IDMSG-ERROR                         
049801*        MUST BE ENTERED                                                  
049901       ELSE                                                               
050001         INSPECT REQU-IDDISTR-KEY REPLACING LEADING SPACE BY ZERO         
050101         IF REQU-IDDISTR-KEY NUMERIC                                      
050201            MOVE REQU-IDDISTR-KEY TO WS-IDDISTR-KEY                       
050301                                     RESP-IDDISTR-KEY                     
050401            INSPECT RESP-IDDISTR-KEY                                      
050501                       REPLACING LEADING ZERO BY SPACE                    
050601         ELSE                                                             
050701           MOVE NOO TO KEYS-SW                                            
050801           MOVE 'IDDISTR'  TO RESP-IDELMT-ERROR                           
050901           MOVE '024'      TO RESP-IDMSG-ERROR                            
051001*          MUST BE NUMERIC                                                
051101         END-IF                                                           
051201       END-IF                                                             
051301     END-IF                                                               
051401                                                                          
051501***  CONTROL OF CUSTOMER                                                  
051601     IF KEYS-OK                                                           
051701       IF REQU-IDKUNDNR-KEY = ALL '+'                                     
051801         MOVE ZERO TO WS-IDKUNDNR-KEY                                     
051901       ELSE                                                               
052001         INSPECT REQU-IDKUNDNR-KEY REPLACING LEADING SPACE BY ZERO        
052101         IF REQU-IDKUNDNR-KEY NUMERIC                                     
052201           MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR-KEY                      
052301                                     RESP-IDKUNDNR-KEY                    
052401         INSPECT RESP-IDKUNDNR-KEY                                        
052501                                   REPLACING LEADING ZERO BY SPACE        
052601         ELSE                                                             
052701           MOVE NOO TO KEYS-SW                                            
052801           MOVE 'IDKUNDNR'  TO RESP-IDELMT-ERROR                          
052901           MOVE '024'       TO RESP-IDMSG-ERROR                           
053001*          MUST BE NUMERIC                                                
053101         END-IF                                                           
053201       END-IF                                                             
053301     END-IF                                                               
053401                                                                          
053501***  CONTROL OF REPORT NO                                                 
053601     IF KEYS-OK                                                           
053701       IF REQU-IDBYTRAP-KEY = ALL '+'                                     
053801         MOVE ZERO TO WS-IDBYTRAP-KEY                                     
053901       ELSE                                                               
054001         INSPECT REQU-IDBYTRAP-KEY REPLACING LEADING SPACE BY ZERO        
054101         IF REQU-IDBYTRAP-KEY NUMERIC                                     
054201            MOVE REQU-IDBYTRAP-KEY TO WS-IDBYTRAP-KEY                     
054301                                      RESP-IDBYTRAP-KEY                   
054401            INSPECT RESP-IDBYTRAP-KEY                                     
054501                                 REPLACING LEADING ZERO BY SPACE          
054601         ELSE                                                             
054701            MOVE NOO TO KEYS-SW                                           
054801            MOVE 'IDBYTRAP'  TO RESP-IDELMT-ERROR                         
054901            MOVE '024'       TO RESP-IDMSG-ERROR                          
055001*           MUST BE NUMERIC                                               
055101         END-IF                                                           
055201       END-IF                                                             
055301     END-IF                                                               
055401                                                                          
055501***  CONTROL OF IDDC                                                      
055601     IF KEYS-OK                                                           
055701       IF REQU-IDDC-KEY NOT = ALL '+'                                     
055801         MOVE REQU-IDDC-KEY TO WS-IDDC-KEY                                
055901         MOVE WS-IDDC-KEY   TO W-IDDC-B6                                  
057001         PERFORM IMS-GU-WDB601                                            
058001         IF SEGMENT-FINNS                                                 
059001           CONTINUE                                                       
061001         ELSE                                                             
061101           MOVE 'IDDC    ' TO RESP-IDELMT-ERROR                           
061201           MOVE '023'    TO RESP-IDMSG-ERROR                              
061301*          IS INVALID                                                     
061401           MOVE NOO TO KEYS-SW                                            
061501         END-IF                                                           
061601       ELSE                                                               
061701          MOVE NOO TO KEYS-SW                                             
061801          MOVE 'IDDC'     TO RESP-IDELMT-ERROR                            
061901          MOVE '026'           TO RESP-IDMSG-ERROR                        
062001*         MUST BE ENTERED                                                 
062101       END-IF                                                             
062201     END-IF                                                               
062301                                                                          
062401     MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                  
062501                                                                          
062601***  CONTROL OF IDDC-REC                                                  
062701     IF KEYS-OK                                                           
062801       IF REQU-IDDC-REC-KEY NOT = ALL '+'                                 
062901         MOVE REQU-IDDC-REC-KEY TO WS-IDDC-REC-KEY                        
063001         MOVE WS-IDDC-REC-KEY   TO W-IDDC-B6                              
063101                                                                          
063201         PERFORM IMS-GU-WDB601                                            
063301         IF SEGMENT-FINNS                                                 
063401           CONTINUE                                                       
063601         ELSE                                                             
063701           MOVE 'IDDC    ' TO RESP-IDELMT-ERROR                           
063801           MOVE '023'    TO RESP-IDMSG-ERROR                              
063901*          IS INVALID                                                     
064001           MOVE NOO TO KEYS-SW                                            
064101         END-IF                                                           
064201       ELSE                                                               
064301          MOVE NOO TO KEYS-SW                                             
064401          MOVE 'IDDC'     TO RESP-IDELMT-ERROR                            
064501          MOVE '026'           TO RESP-IDMSG-ERROR                        
064601*         MUST BE ENTERED                                                 
064701       END-IF                                                             
064801     END-IF                                                               
064901                                                                          
065001     MOVE REQU-IDDC-REC-KEY TO RESP-IDDC-REC-KEY                          
065101                                                                          
065201     IF KEYS-OK                                                           
065301       MOVE SPACE TO WS-STYR-LAS                                          
065401       IF WS-IDDISTR-KEY NOT = ZERO                                       
065501         MOVE 'D' TO WS-DISTR-STYR                                        
065601       END-IF                                                             
065701       IF WS-IDKUNDNR-KEY NOT = ZERO                                      
065801         MOVE 'K' TO WS-KUND-STYR                                         
065901       END-IF                                                             
066001       IF WS-IDBYTRAP-KEY NOT = ZERO                                      
066101         MOVE 'R' TO WS-RAPP-STYR                                         
066201       END-IF                                                             
066301       IF WS-KDBYTSTA-KEY = '2'                                           
066401         MOVE '2' TO WS-STATUS-STYR                                       
066501       END-IF                                                             
066601                                                                          
066701       IF WS-STYR-LAS =                                                   
066801          'D  2' OR                                                       
066901          'D R2' OR                                                       
067001          'DK 2' OR                                                       
067101          'DKR2'                                                          
067201          CONTINUE                                                        
067301       ELSE                                                               
067401           MOVE NOO TO KEYS-SW                                            
067501           MOVE 'KEYS    ' TO RESP-IDELMT-ERROR                           
067601           MOVE '043'      TO RESP-IDMSG-ERROR                            
067701*          INVALID KEY FIELDS                                             
067801       END-IF                                                             
067901     END-IF                                                               
068001                                                                          
068101     IF KEYS-OK                                                           
068201       MOVE YES             TO WDGX3148-SW                                
068301       MOVE REQU-IDUSER     TO W-3148-IDUSER                              
068401       MOVE WS-IDDC-KEY     TO W-3148-IDDC                                
068402       MOVE WS-IDDC-REC-KEY TO W-3148-IDDC-REC                            
068501       PERFORM IMS-GU-WDGX3148                                            
068601       IF SEGMENT-FINNS                                                   
068701          IF 3148-FLKLAR = NOO                                            
068801            CONTINUE                                                      
068901          ELSE                                                            
069001            MOVE NOO TO KEYS-SW                                           
069101            MOVE 'KEYS    ' TO RESP-IDELMT-ERROR                          
069201            MOVE '115'      TO RESP-IDMSG-ERROR                           
069301          END-IF                                                          
069401       ELSE                                                               
069501          MOVE NOO        TO WDGX3148-SW                                  
069601       END-IF                                                             
069701     END-IF                                                               
069801     .                                                                    
069901      EJECT                                                               
070001                                                                          
080001 C-UPDATE-WDGX3150 SECTION.                                               
090001                                                                          
091001     MOVE 'C-UPDATE-WDGX3150'    TO CURRENT-SECTION                       
092001     MOVE YES TO FORSTA-SW                                                
092101     MOVE +1 TO INDX                                                      
092201     PERFORM UNTIL INDX > REQU-KVRADER                                    
092301       MOVE REQU-IDDISTR (INDX)      TO W-3150-IDDISTR                    
092401       MOVE REQU-IDBYTRAP(INDX)      TO W-3150-IDBYTRAP                   
092501       IF REQU-KDCMD-RAD (INDX) = 'J'                                     
092601          IF FORSTA-OK                                                    
092701            IF WDGX3148-OK                                                
092801               CONTINUE                                                   
092901            ELSE                                                          
093001               MOVE W-3148-IDUSER       TO 3148-IDUSER                    
093101               MOVE W-3148-IDDC         TO 3148-IDDC                      
093102               MOVE W-3148-IDDC-REC     TO 3148-IDDC-REC                  
093201               MOVE NOO                 TO 3148-FLKLAR                    
093301               MOVE ZERO                TO 3148-VKORDBTO                  
093401               MOVE ZERO                TO 3148-VLORDBTO                  
093501               MOVE DAGENS-DATUM-AAMMDD TO 3148-TIREGDAT                  
093601               MOVE DAGENS-TID          TO 3148-TIKLOCK                   
093701               PERFORM IMS-ISRT-WDGX3148                                  
093801            END-IF                                                        
093901            MOVE NOO TO FORSTA-SW                                         
094001          END-IF                                                          
094101          PERFORM IMS-GU-WDGX3150                                         
094201          IF SEGMENT-FINNS                                                
094301             CONTINUE                                                     
094401          ELSE                                                            
094501             MOVE W-3150-IDDISTR    TO 3150-IDDISTR                       
094601             MOVE W-3150-IDBYTRAP   TO 3150-IDBYTRAP                      
094701             PERFORM IMS-ISRT-WDGX3150                                    
094801          END-IF                                                          
094901       ELSE                                                               
095001          IF WDGX3148-OK                                                  
095101             PERFORM IMS-GHU-WDGX3150                                     
095201             IF SEGMENT-FINNS                                             
095301                PERFORM IMS-DLET-WDGX3150                                 
095401             END-IF                                                       
095501          END-IF                                                          
095601       END-IF                                                             
095701       ADD +1 TO INDX                                                     
095801     END-PERFORM                                                          
095901                                                                          
096001     .                                                                    
096101      EJECT                                                               
096201                                                                          
096301 D-VIEW-SHIPDOC SECTION.                                                  
096401                                                                          
096501     MOVE 'D-VIEW-SHIPDOC'    TO CURRENT-SECTION                          
096601     MOVE +1 TO INDX                                                      
096701     PERFORM IMS-GU-WDGX3148-2                                            
096801     PERFORM IMS-GNP-WDGX3150                                             
096901     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                        
097001                   INDX > MAX-INDX                                        
097101        IF 3150-IDDISTR = WS-IDDISTR-KEY                                  
097201          MOVE 3150-IDDISTR  TO W-IDDISTR-UNIK                            
097301          MOVE 3150-IDBYTRAP TO W-IDBYTRAP-UNIK                           
097401          PERFORM IMS-GU-WDM601                                           
097501          PERFORM DA-DATA-TO-RESP                                         
097601          ADD +1 TO INDX                                                  
097701        END-IF                                                            
097801        PERFORM IMS-GNP-WDGX3150                                          
097901     END-PERFORM                                                          
098001     COMPUTE INDX = INDX - 1                                              
098101     IF INDX > ZERO                                                       
098201       MOVE 3148-VKORDBTO  TO RESP-VKORDBTO                               
098301       MOVE 3148-VLORDBTO  TO RESP-VLORDBTO                               
098401     END-IF                                                               
098501     MOVE INDX TO RESP-KVRADER                                            
098601     .                                                                    
098701     EJECT                                                                
098801                                                                          
098901 DA-DATA-TO-RESP SECTION.                                                 
099000                                                                          
099101     MOVE 'DA-DATA-TO-RESP' TO CURRENT-SECTION                            
099201     MOVE RAPP-IDDISTR             TO RESP-IDDISTR    (INDX)              
099301     MOVE RAPP-IDKUNDNR            TO RESP-IDKUNDNR   (INDX)              
099401     MOVE RAPP-IDFAKT              TO RESP-IDFAKT     (INDX)              
099501     MOVE RAPP-IDBYTRAP            TO RESP-IDBYTRAP   (INDX)              
099601     MOVE RAPP-KVRETUR-TOT         TO RESP-KVRETUR-TOT(INDX)              
099701     MOVE RAPP-DAREGDAT(3:6)       TO RESP-TIREGDAT   (INDX)              
099801     .                                                                    
099901      EJECT                                                               
100001                                                                          
100101 F-READ-SHOW-INFO SECTION.                                                
100201                                                                          
100301     MOVE 'F-READ-SHOW-INFO' TO CURRENT-SECTION                           
100401                                                                          
100501     MOVE WS-IDDC-REC-KEY    TO W-IDDC-D1                                 
100601                                W-IDDC-D1-MAX                             
100701                                                                          
100801     IF WS-IDDISTR-KEY  NOT = ZERO                                        
100901       MOVE WS-IDDISTR-KEY   TO W-IDDISTR-D1                              
101001                                W-IDDISTR-D1-MAX                          
102001                                W-3150-IDDISTR                            
103001     END-IF                                                               
104001                                                                          
105001     IF WS-IDKUNDNR-KEY NOT = ZERO                                        
106001        MOVE WS-IDKUNDNR-KEY TO W-IDKUNDNR                                
106101     END-IF                                                               
106201                                                                          
106301     IF WS-IDBYTRAP-KEY NOT = ZERO                                        
106401       MOVE WS-IDBYTRAP-KEY  TO W-IDBYTRAP                                
106501     END-IF                                                               
106601                                                                          
106701     IF WS-KDBYTSTA-KEY NOT = ZERO                                        
106801       MOVE WS-KDBYTSTA-KEY TO W-KDBYTSTA-D1                              
106901                               W-KDBYTSTA-D1-MAX                          
107001     END-IF                                                               
108001                                                                          
109001     PERFORM FA-READ-DB                                                   
109101     IF SEGMENT-SAKNAS                                                    
109201       MOVE 'REPORT' TO RESP-IDELMT-ERROR                                 
109301       MOVE '027'      TO RESP-IDMSG-ERROR                                
109401     ELSE                                                                 
109501       MOVE +1 TO INDX                                                    
109601                                                                          
109701       PERFORM UNTIL INDX > MAX-INDX                                      
109801         IF SEGMENT-FINNS                                                 
109901           MOVE RAPP-IDBYTRAP TO W-3150-IDBYTRAP                          
110001           IF WDGX3148-OK                                                 
110101             PERFORM IMS-GU-WDGX3150                                      
110201             IF SEGMENT-FINNS                                             
110301               MOVE YES   TO RESP-KDCMD-RAD (INDX)                        
110401             ELSE                                                         
110501               MOVE SPACE TO RESP-KDCMD-RAD (INDX)                        
110601             END-IF                                                       
110701           ELSE                                                           
110801             MOVE SPACE TO RESP-KDCMD-RAD (INDX)                          
110901           END-IF                                                         
111001           PERFORM FB-DATA-TO-RESP                                        
111101           PERFORM FA-READ-DB                                             
111201         ELSE                                                             
111301           MOVE MAX-INDX TO INDX                                          
111401         END-IF                                                           
111501         ADD 1 TO INDX                                                    
111601       END-PERFORM                                                        
111701       MOVE WS-KVRADER     TO RESP-KVRADER                                
111801     END-IF                                                               
111901     .                                                                    
112001     EJECT                                                                
112101                                                                          
112201 FA-READ-DB SECTION.                                                      
112301                                                                          
112401     MOVE 'FA-READ-DB' TO CURRENT-SECTION                                 
112501     EVALUATE WS-STYR-LAS                                                 
112601       WHEN 'D  2 '                                                       
112701         PERFORM IMS-GN-WDM6D1                                            
112801         IF SEGMENT-FINNS                                                 
112901                                                                          
113001           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
114001           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
115001           PERFORM IMS-GU-WDM601                                          
115101         END-IF                                                           
115201       WHEN 'DK 2'                                                        
115301         PERFORM IMS-GN-WDM6D1-KUN                                        
115401         IF SEGMENT-FINNS                                                 
115501           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
115601           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
115701           PERFORM IMS-GU-WDM601                                          
115801         END-IF                                                           
115901       WHEN 'D R2'                                                        
116001         PERFORM IMS-GN-WDM6D1-RAP                                        
116101         IF SEGMENT-FINNS                                                 
116201           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
116301           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
116401           PERFORM IMS-GU-WDM601                                          
116501         END-IF                                                           
116601       WHEN 'DKR2'                                                        
116701         PERFORM IMS-GN-WDM6D1-KUN-RAP                                    
116801         IF SEGMENT-FINNS                                                 
116901           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
117001           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
117101           PERFORM IMS-GU-WDM601                                          
117201         END-IF                                                           
117301     END-EVALUATE                                                         
117401     .                                                                    
117501     EJECT                                                                
117601                                                                          
117701 FB-DATA-TO-RESP SECTION.                                                 
117801                                                                          
117901     MOVE 'FB-DATA-TO-RESP' TO CURRENT-SECTION                            
118001     MOVE RAPP-IDDISTR             TO RESP-IDDISTR    (INDX)              
118101     MOVE RAPP-IDKUNDNR            TO RESP-IDKUNDNR   (INDX)              
118201     MOVE RAPP-IDFAKT              TO RESP-IDFAKT     (INDX)              
118301     MOVE RAPP-IDBYTRAP            TO RESP-IDBYTRAP   (INDX)              
118401     MOVE RAPP-KVRETUR-TOT         TO RESP-KVRETUR-TOT(INDX)              
118501     MOVE RAPP-DAREGDAT(3:6)       TO RESP-TIREGDAT   (INDX)              
118601     ADD  1                        TO WS-KVRADER                          
118701     .                                                                    
118801     EJECT                                                                
118901                                                                          
119001                                                                          
120001 G-CHECK-INPUT SECTION.                                                   
121001                                                                          
122001     MOVE 'G-CHECK-INPUT' TO CURRENT-SECTION                              
123001     MOVE YES                    TO INDATA-SW                             
124001     MOVE NOO                    TO CHANGE-WEIGHT-SW                      
125001     MOVE NOO                    TO CHANGE-VOLUME-SW                      
126001     IF REQU-VKORDBTO NOT = ALL '+' OR                                    
126101        REQU-VLORDBTO NOT = ALL '+'                                       
126201        PERFORM GA-CHECK-INPUT                                            
126301     END-IF                                                               
126401     IF INDATA-OK                                                         
126501       IF REQU-FLKLAR = YES                                               
126601         IF CHANGE-NO-WEIGHT AND                                          
126701            CHANGE-NO-VOLUME                                              
126801           PERFORM GB-CHECK-FLKLAR                                        
126901         ELSE                                                             
127001           MOVE NOO              TO INDATA-SW                             
127101           MOVE SPACE            TO RESP-IDELMT-ERROR                     
127201           MOVE '319'            TO RESP-IDMSG-ERROR                      
127301         END-IF                                                           
127401       END-IF                                                             
127501     END-IF                                                               
127601     .                                                                    
127701     EJECT                                                                
127801                                                                          
127901 GA-CHECK-INPUT SECTION.                                                  
128001                                                                          
128101     MOVE 'GA-CHECK-INPUT' TO CURRENT-SECTION                             
128201     PERFORM IMS-GHU-WDGX3148                                             
128301     IF REQU-VKORDBTO NOT = ALL '+'                                       
128401       MOVE REQU-VKORDBTO       TO DEC-IDFRIDATA                          
128501       MOVE +6                  TO DEC-KVHELTAL                           
128601       MOVE +1                  TO DEC-KVDECIMAL                          
128701                                                                          
128801       CALL WDECEDIT USING DEC-WDECAREA                                   
128901       IF DEC-KDSVAR-OK                                                   
129001          MOVE DEC-IDEDITDATA   TO WS-VKORDBTO                            
129101          IF WS-VKORDBTO = 3148-VKORDBTO                                  
129201            CONTINUE                                                      
129301          ELSE                                                            
129401            MOVE YES TO CHANGE-WEIGHT-SW                                  
129501          END-IF                                                          
129601       ELSE                                                               
129701          MOVE NOO TO INDATA-SW                                           
129801          MOVE 'VKART   '       TO RESP-IDELMT-ERROR                      
129901          MOVE '023'            TO RESP-IDMSG-ERROR                       
130001       END-IF                                                             
130101     END-IF                                                               
130201                                                                          
130301     IF REQU-VLORDBTO NOT = ALL '+'                                       
130401       MOVE REQU-VLORDBTO       TO DEC-IDFRIDATA                          
130501       MOVE +4                  TO DEC-KVHELTAL                           
130601       MOVE +3                  TO DEC-KVDECIMAL                          
130701                                                                          
130801       CALL WDECEDIT USING DEC-WDECAREA                                   
130901       IF DEC-KDSVAR-OK                                                   
131001          MOVE DEC-IDEDITDATA   TO WS-VLORDBTO                            
131101          IF WS-VLORDBTO = 3148-VLORDBTO                                  
131201            CONTINUE                                                      
131301          ELSE                                                            
131401            MOVE YES TO CHANGE-VOLUME-SW                                  
131501          END-IF                                                          
131601       ELSE                                                               
131701          MOVE NOO TO INDATA-SW                                           
131801          MOVE 'VLART   '       TO RESP-IDELMT-ERROR                      
131901          MOVE '023'            TO RESP-IDMSG-ERROR                       
132001       END-IF                                                             
132101     END-IF                                                               
132201     .                                                                    
132301     EJECT                                                                
132401                                                                          
132501 GB-CHECK-FLKLAR SECTION.                                                 
132601                                                                          
132701     MOVE 'GB-CHECK-FLKLAR' TO CURRENT-SECTION                            
132801     IF REQU-VKORDBTO = ALL '+' AND                                       
132901        REQU-VLORDBTO = ALL '+'                                           
133001       PERFORM IMS-GHU-WDGX3148                                           
133101     END-IF                                                               
133201     IF SEGMENT-FINNS                                                     
133301        IF 3148-VKORDBTO > ZERO AND                                       
133401           3148-VLORDBTO > ZERO                                           
133501           CONTINUE                                                       
133601        ELSE                                                              
133701           MOVE NOO TO INDATA-SW                                          
133801           IF 3148-VKORDBTO = ZERO                                        
133901             MOVE 'VKART   '    TO RESP-IDELMT-ERROR                      
134001             MOVE '026'         TO RESP-IDMSG-ERROR                       
134101           ELSE                                                           
134201             MOVE 'VLART   '    TO RESP-IDELMT-ERROR                      
134301             MOVE '026'         TO RESP-IDMSG-ERROR                       
134401           END-IF                                                         
134501        END-IF                                                            
134601     END-IF                                                               
134701     .                                                                    
134801     EJECT                                                                
134901                                                                          
135001                                                                          
135101 H-UPDATE SECTION.                                                        
135201                                                                          
135301     MOVE 'H-UPDATE' TO CURRENT-SECTION                                   
135401     IF REQU-FLKLAR = YES                                                 
135501       PERFORM HA-CREATE-SHIPDOC                                          
135601       MOVE YES TO 3148-FLKLAR                                            
135701       PERFORM IMS-REPL-WDGX3148                                          
135801       MOVE SPACE          TO RESP-IDELMT-ERROR                           
135901       MOVE '376'          TO RESP-IDMSG-INFO                             
136001     ELSE                                                                 
136101       PERFORM IMS-GHU-WDGX3148                                           
136201       MOVE WS-VKORDBTO TO 3148-VKORDBTO                                  
136301       MOVE WS-VLORDBTO TO 3148-VLORDBTO                                  
136401       PERFORM IMS-REPL-WDGX3148                                          
136501       MOVE SPACE          TO RESP-IDELMT-ERROR                           
136601       MOVE '001'          TO RESP-IDMSG-INFO                             
136701     END-IF                                                               
136801     .                                                                    
136901     EJECT                                                                
137001                                                                          
137101                                                                          
137201 HA-CREATE-SHIPDOC SECTION.                                               
137301                                                                          
137401     MOVE 'HA-CREATE-SHIPDOC' TO CURRENT-SECTION                          
137501                                                                          
137601     MOVE 'L168'                 TO MSGSOP-IDTRANS                        
137701     MOVE '1'                    TO MSGSOP-KDMFSFOR                       
137801     MOVE 'O'                    TO MSGSOP-KDSOPFUNK                      
137901     MOVE 'W371S2'               TO MSGSOP-IDPROCESS                      
138001     STRING 'IDUSER(' REQU-IDUSER                                         
138101            ') IDDC(' REQU-IDDC-KEY                                       
138201            ') IDDISTR(' WS-IDDISTR-KEY                                   
138301            ') IDDC-REC(' REQU-IDDC-REC-KEY                               
138401            ')'                                                           
138501             DELIMITED BY SIZE INTO MSGSOP-TESYMBV                        
138601     PERFORM IMS-ISRT-ALT-MSG                                             
138701     .                                                                    
138801     EJECT                                                                
138901                                                                          
139001                                                                          
139101 Z-FINIT SECTION.                                                         
139201                                                                          
139301     MOVE 'Z-FINIT' TO CURRENT-SECTION                                    
139401     CONTINUE                                                             
139501     .                                                                    
139601     EJECT                                                                
139701*    --- DISPATCHER SECTIONS                                              
139801 S01-FETCH-REQUDATA SECTION.                                              
139901                                                                          
140001     MOVE 'S01-FETCH-REQUDATA'   TO CURRENT-SECTION                       
140101     MOVE 'GETARG'               TO SUB-KDFUNC                            
140201     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
140301     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
140401                                                                          
140501     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
140601                                                                          
140701     IF SUB-KDRC > 0                                                      
140801       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
140901       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
141001       DELIMITED BY SIZE INTO ERROR-TEXT                                  
141101       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
141201     END-IF                                                               
141301     .                                                                    
141401     SKIP3                                                                
141501 S02-RETURN-ANSWER SECTION.                                               
141601                                                                          
141701     MOVE 'S02-RETURN-ANSWER' TO CURRENT-SECTION                          
141801     COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA                           
141901     MOVE 'RETURN'                    TO SUB-KDFUNC                       
142001     MOVE WS-RESP-AREA                TO SUB-KVDLEN                       
142101                                                                          
142201     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
142301                                                                          
142401     IF SUB-KDRC > 0                                                      
142501       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
142601       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
142701       DELIMITED BY SIZE INTO ERROR-TEXT                                  
142801       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
142901     END-IF                                                               
143001     .                                                                    
143101     EJECT                                                                
143201* --- IMS SEKTIONER ---                                                   
143301 IMS-ISRT-ALT-MSG SECTION.                                                
143401                                                                          
143501     MOVE SPACE                  TO GODK-STATUSKODER                      
143601     CALL CBLTDLI USING ISRT ALT-PCB MSGSOP-WMSGSOP                       
143701     MOVE ALT-STATUS-CODE        TO STATUS-WS                             
143801     PERFORM IMS-STATUSKONTROLL                                           
143901     .                                                                    
144001     EJECT                                                                
144101 IMS-GN-WDM6D1  SECTION.                                                  
145001       MOVE 'GN-WDM6D1' TO CURRENT-IMS-SECTION                            
146001     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
147001                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X ')'                    
148001          DELIMITED BY SIZE INTO SSA1                                     
148101     MOVE '  GBGE' TO GODK-STATUSKODER                                    
148201     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
148301     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
148401     PERFORM IMS-STATUSKONTROLL                                           
148501     .                                                                    
148601     SKIP2                                                                
148701 IMS-GN-WDM6D1-RAP  SECTION.                                              
148801       MOVE 'GN-WDM6D1-RAP' TO CURRENT-IMS-SECTION                        
148901     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
149001                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
149101                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
149201          DELIMITED BY SIZE INTO SSA1                                     
149301     MOVE '  GBGE' TO GODK-STATUSKODER                                    
149401     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
149501     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
149601     PERFORM IMS-STATUSKONTROLL                                           
149701     .                                                                    
149801     EJECT                                                                
149901 IMS-GN-WDM6D1-KUN  SECTION.                                              
150001       MOVE 'GN-WDM6D1-KUN' TO CURRENT-IMS-SECTION                        
150101     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
150201                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
150301                    '&IDKUNDNR =' W-IDKUNDNR-X ')'                        
150401          DELIMITED BY SIZE INTO SSA1                                     
150501     MOVE '  GBGE' TO GODK-STATUSKODER                                    
150601     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
150701     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
150801     PERFORM IMS-STATUSKONTROLL                                           
150901     .                                                                    
151001     SKIP2                                                                
151101 IMS-GN-WDM6D1-KUN-RAP  SECTION.                                          
151201       MOVE 'GN-WDM6D1-KUN-RAP' TO CURRENT-IMS-SECTION                    
151301     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
151401                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
151501                    '&IDKUNDNR =' W-IDKUNDNR-X                            
151601                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
151701          DELIMITED BY SIZE INTO SSA1                                     
151801     MOVE '  GBGE' TO GODK-STATUSKODER                                    
151901     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
152001     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
152101     PERFORM IMS-STATUSKONTROLL                                           
152201     .                                                                    
152301     SKIP2                                                                
152401 IMS-GU-WDM601 SECTION.                                                   
152501       MOVE 'GU-WDM601' TO CURRENT-IMS-SECTION                            
152601     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
152701          DELIMITED BY SIZE INTO SSA1                                     
152801     MOVE '  ' TO GODK-STATUSKODER                                        
152901     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM601 SSA1                    
153001     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
154001     PERFORM IMS-STATUSKONTROLL                                           
154101     .                                                                    
154201     SKIP2                                                                
154301 IMS-GNP-WDM611  SECTION.                                                 
154401       MOVE 'GNP-WDM611' TO CURRENT-IMS-SECTION                           
154501                                                                          
154601     MOVE   'WDM611'          TO SSA1                                     
154701     MOVE '  GEGB' TO GODK-STATUSKODER                                    
154801     CALL CBLTDLI USING GNP  WDM6-PCB DLI-IO-WDM611 SSA1                  
154901     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
155001     PERFORM IMS-STATUSKONTROLL                                           
156001     .                                                                    
156101     EJECT                                                                
156201 IMS-GU-WDB601    SECTION.                                                
156301     MOVE 'IMS-GU-WDB601'        TO CURRENT-IMS-SECTION                   
156401     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
156501          DELIMITED BY SIZE INTO SSA1                                     
156601     MOVE '  GE' TO GODK-STATUSKODER                                      
156701     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
156801     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
156901     PERFORM IMS-STATUSKONTROLL                                           
157001     IF SEGMENT-SAKNAS                                                    
157101         MOVE SPACE TO DCS-KDDC                                           
157201     END-IF                                                               
157301     .                                                                    
157401     EJECT                                                                
157501 IMS-GU-WDGX3148 SECTION.                                                 
157601                                                                          
157701     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
157801             DELIMITED BY SIZE INTO SSA1                                  
157901     STRING 'WDGX3148(KY3148   =' W-3148KY-X ')'                          
158001             DELIMITED BY SIZE INTO SSA2                                  
158101     MOVE '  GE'                 TO GODK-STATUSKODER                      
158201     CALL CBLTDLI USING GU 3147-PCB DLI-IO-WDGX3148 SSA1 SSA2             
158301     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
158401     PERFORM IMS-STATUSKONTROLL                                           
158501     .                                                                    
158601     EJECT                                                                
158701 IMS-GHU-WDGX3148 SECTION.                                                
158801                                                                          
158901     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
159001             DELIMITED BY SIZE INTO SSA1                                  
159101     STRING 'WDGX3148(KY3148   =' W-3148KY-X ')'                          
159201             DELIMITED BY SIZE INTO SSA2                                  
159301     MOVE '  GE'                 TO GODK-STATUSKODER                      
159401     CALL CBLTDLI USING GHU 3147-PCB DLI-IO-WDGX3148 SSA1 SSA2            
159501     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
159601     PERFORM IMS-STATUSKONTROLL                                           
159701     .                                                                    
159801     EJECT                                                                
159901 IMS-GU-WDGX3148-2 SECTION.                                               
160001                                                                          
160101     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
160201             DELIMITED BY SIZE INTO SSA1                                  
160301     STRING 'WDGX3148(KY3148   =' W-3148KY-X ')'                          
160401             DELIMITED BY SIZE INTO SSA2                                  
160500     MOVE '  '                 TO GODK-STATUSKODER                        
160601     CALL CBLTDLI USING GU 3147-PCB DLI-IO-WDGX3148 SSA1 SSA2             
160701     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
160801     PERFORM IMS-STATUSKONTROLL                                           
160901     .                                                                    
161001     EJECT                                                                
161101 IMS-GU-WDGX3150 SECTION.                                                 
161200                                                                          
161300     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
161400             DELIMITED BY SIZE INTO SSA1                                  
161500     STRING 'WDGX3148(KY3148   =' W-3148KY-X ')'                          
161600             DELIMITED BY SIZE INTO SSA2                                  
161700     STRING 'WDGX3150(KY3150   =' W-3150KY-X ')'                          
161800             DELIMITED BY SIZE INTO SSA3                                  
161900     MOVE '  GE'                 TO GODK-STATUSKODER                      
162000     CALL CBLTDLI USING GU 3147-PCB DLI-IO-WDGX3150 SSA1 SSA2 SSA3        
162100     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
162200     PERFORM IMS-STATUSKONTROLL                                           
162300     .                                                                    
162401     EJECT                                                                
162501 IMS-GHU-WDGX3150 SECTION.                                                
162601                                                                          
162701     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
162801             DELIMITED BY SIZE INTO SSA1                                  
162901     STRING 'WDGX3148(KY3148   =' W-3148KY-X ')'                          
163001             DELIMITED BY SIZE INTO SSA2                                  
163101     STRING 'WDGX3150(KY3150   =' W-3150KY-X ')'                          
163201             DELIMITED BY SIZE INTO SSA3                                  
163301     MOVE '  GE'                 TO GODK-STATUSKODER                      
163401     CALL CBLTDLI USING GHU 3147-PCB DLI-IO-WDGX3150                      
163501                            SSA1 SSA2 SSA3                                
163601     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
163701     PERFORM IMS-STATUSKONTROLL                                           
163801     .                                                                    
163901     EJECT                                                                
164001 IMS-GNP-WDGX3150 SECTION.                                                
164101                                                                          
164201     MOVE 'WDGX3150 '            TO SSA1                                  
164301     MOVE '  GEGB'               TO GODK-STATUSKODER                      
164401     CALL CBLTDLI USING GNP 3147-PCB DLI-IO-WDGX3150 SSA1                 
164501     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
164601     PERFORM IMS-STATUSKONTROLL                                           
164701     .                                                                    
164801     EJECT                                                                
164900 IMS-ISRT-WDGX3148 SECTION.                                               
165000                                                                          
165100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
165200             DELIMITED BY SIZE INTO SSA1                                  
165300     MOVE 'WDGX3148 '            TO SSA2                                  
165400     MOVE '  '                   TO GODK-STATUSKODER                      
165500     CALL CBLTDLI USING ISRT 3147-PCB DLI-IO-WDGX3148 SSA1 SSA2           
165600     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
165700     PERFORM IMS-STATUSKONTROLL                                           
165800     .                                                                    
165901     EJECT                                                                
166001 IMS-REPL-WDGX3148 SECTION.                                               
166101                                                                          
166201     MOVE 'WDGX3148 '            TO SSA1                                  
166301     MOVE '  '                   TO GODK-STATUSKODER                      
166401     CALL CBLTDLI USING REPL 3147-PCB DLI-IO-WDGX3148 SSA1                
166501     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
166601     PERFORM IMS-STATUSKONTROLL                                           
166701     .                                                                    
166800     EJECT                                                                
166900 IMS-ISRT-WDGX3150 SECTION.                                               
167000                                                                          
167100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
167200             DELIMITED BY SIZE INTO SSA1                                  
167301     STRING 'WDGX3148(KY3148   =' W-3148KY-X ')'                          
167401             DELIMITED BY SIZE INTO SSA2                                  
167500     MOVE 'WDGX3150 '            TO SSA3                                  
167600     MOVE '  '                   TO GODK-STATUSKODER                      
167700     CALL CBLTDLI USING ISRT 3147-PCB DLI-IO-WDGX3150                     
167801                             SSA1 SSA2 SSA3                               
167900     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
168000     PERFORM IMS-STATUSKONTROLL                                           
168100     .                                                                    
168200     EJECT                                                                
168300 IMS-DLET-WDGX3150 SECTION.                                               
168400                                                                          
168500     MOVE 'WDGX3150 '            TO SSA1                                  
168600     MOVE '  '                   TO GODK-STATUSKODER                      
168700     CALL CBLTDLI USING DLET 3147-PCB DLI-IO-WDGX3150 SSA1                
168800     MOVE 3147-STATUS-CODE       TO STATUS-WS                             
168900     PERFORM IMS-STATUSKONTROLL                                           
169000     .                                                                    
169101     SKIP3                                                                
169201 IMS-STATUSKONTROLL SECTION.                                              
169301                                                                          
169401     SET STATUS-IX TO 1                                                   
169501     SEARCH GODK-STATUS                                                   
169601       AT END                                                             
169701         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
169801         DELIMITED BY SIZE INTO ERROR-TEXT                                
169901         CALL FELLOG                                                      
170001       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
170101         CONTINUE                                                         
170201     END-SEARCH                                                           
170301     .                                                                    
