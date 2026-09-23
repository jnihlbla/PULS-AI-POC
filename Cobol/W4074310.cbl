010099*COMPOPT STDSUB=YES                                                       
020000 ID DIVISION.                                                             
030000     SKIP2                                                                
040099 PROGRAM-ID.     W4074310.                                                
050000*AUTHOR.         LARS THELL.                                              
060099*DATE-WRITTEN.   95/07/20.                                                
070000                                                                          
080000*    REMARKS.                                                             
090000*                                                                         
100000*    FUNKTION:                                                            
110099*        SUBPROGRAM SOM SKRIVER UT LISTA MED INGÅENDE                     
120099*        KOLLIN OCH RETURTILLSTÅND FÖR EN SÄNDNING.                       
130099*        STARTAS AV W4074300                                              
140000*                                                                         
150081*        PROGRAMMET          LÄSER      WLRETA (WDA3)                     
160000*                                                                         
160100*    E'TRACKER: 5166760  2007-06-12                                       
160200*                                                                         
170000                                                                          
180000     SKIP3                                                                
190000 ENVIRONMENT DIVISION.                                                    
200000     EJECT                                                                
210000 DATA DIVISION.                                                           
220000 WORKING-STORAGE SECTION.                                                 
220199                                                                          
221099*    -- CHECKED BY WY2000                                                 
230099 77  IDPGM                       PIC X(08)   VALUE 'W4074310'.            
240000                                                                          
250000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
260000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
270000                                                                          
280000 77  JA                          PIC X       VALUE 'J'.                   
290000 77  NEJ                         PIC X       VALUE 'N'.                   
300006                                                                          
310099 77  W-SPAR-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.           
320099 77  MAX-KVRADER                 PIC S9(3)   VALUE +40 COMP-3.            
330099 77  W-KVRADER                   PIC S9(3)   VALUE ZERO COMP-3.           
331099 77  W-KVKOLLI                   PIC S9(3)   VALUE ZERO COMP-3.           
340099 77  W-IDSIDNR                   PIC S9(3)   VALUE 0   COMP-3.            
350099                                                                          
360000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
370000 01  GENERELLA-SUBPROGRAM.                                                
380000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
390000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
401099     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
410000     SKIP3                                                                
420067* VARIABLER TILL SUBPROGRAM W006PRR1                                      
421099 01  W006-W006PRR1-AREA          PIC X(24)   VALUE                        
422099                                            'W006PRR1 AREA  '.            
430000*01  -COPY W006PRAR                                                       
440000     SKIP2                                                                
450000     EJECT                                                                
460002 01  WS-RAPP-AREA.                                                        
470080     03  WS-RAPP-PRINTER         PIC X(8).                                
471099     03 WS-PRT-IDLIST.                                                    
472099        05 WS-IDLIST             PIC X(7)   VALUE 'SHIPPIN'.              
473099        05 WS-PRT-LOPNR          PIC 9(3)   VALUE ZERO.                   
480002     03  WS-RAPP-LISTRAD.                                                 
490080         05  FILLER              PIC X(1)    VALUE SPACE.                 
500080         05  WS-RAPP-RAD         PIC X(120).                              
510002     03  WS-DUMMY                PIC X(1).                                
520002     EJECT                                                                
530000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
540000*                                                                         
550000     EJECT                                                                
560000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
570000     SKIP3                                                                
580000 01  NYCKLAR-TILL-DLI.                                                    
590080                                                                          
600099     03  W-WDA3BSEQ-MIN-X.                                                
611099         05  W-IDRT-BSEQ-MIN      PIC  X(3)          VALUE SPACE.         
611100         05  W-IDDC-BSEQ-MIN      PIC  X(2)          VALUE SPACE.         
620099         05  W-IDRTLOP-BSEQ-MIN   PIC  9(3)          VALUE ZERO.          
630099         05  W-IDKOLLI-BSEQ-MIN   PIC S9(5)   COMP-3 VALUE ZERO.          
640099                                                                          
650099     03  W-WDA3BSEQ-MAX-X.                                                
661099         05  W-IDRT-BSEQ-MAX      PIC  X(3)          VALUE SPACE.         
661100         05  W-IDDC-BSEQ-MAX      PIC  X(2)          VALUE SPACE.         
670099         05  W-IDRTLOP-BSEQ-MAX   PIC  9(3)          VALUE ZERO.          
680099         05  W-IDKOLLI-BSEQ-MAX   PIC S9(5)   COMP-3 VALUE ZERO.          
690081                                                                          
700000     SKIP2                                                                
710000*    --- STATUS-KOD FRÅN IMS                                              
720000 01  STATUS-WS                   PIC XX.                                  
730000     88  SEGMENT-FINNS                       VALUE '  '.                  
740000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
750000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
760003     88  SEGMENT-SLUT                        VALUE 'GB'.                  
770000     SKIP2                                                                
780000 01  GODK-STATUSKODER.                                                    
790000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
800000     SKIP3                                                                
810012 01  SSA1                        PIC X(96).                               
820000     EJECT                                                                
830000*    --- IMS FUNKTIONSKODER                                               
840000*01  -COPY W0003                                                          
850000     EJECT                                                                
860000*    ---  DLI INPUT-OUTPUT AREA                                           
870081 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
880099                                                                          
890003 01  DLI-IO-AREA1.                                                        
900078     03  IO-AREA1                PIC X(500)  VALUE SPACE.                 
910099     03  WLRETA01 REDEFINES IO-AREA1.                                     
920099*        05  -COPY WDA301                                                 
930099     EJECT                                                                
940099*  PRINTRADER FÖR SÄNDNINGSLISTA                                          
950002                                                                          
960002 01  LIST-HRAD1.                                                          
970002     03   FILLER                  PIC X(1)  VALUE SPACE.                  
980045     03   FILLER                  PIC X(22) VALUE                         
990045                                        'VOLVO CAR AFTER SALES '.         
000091     03   FILLER                  PIC X(9)  VALUE SPACE.                  
010099     03   FILLER                  PIC X(6)  VALUE 'W40743'.               
020099     03   FILLER                  PIC X(6)  VALUE SPACE.                  
030099     03   FILLER                  PIC X(35) VALUE                         
040099                           'INCLUDING CASES IN RETURN TRANSFER'.          
050099     03   FILLER                  PIC X(18) VALUE SPACE.                  
060008     03   HRAD1-DATUM             PIC X(6)  VALUE SPACE.                  
070099     03   FILLER                  PIC X(7)  VALUE SPACE.                  
080099     03   FILLER                  PIC X(5)  VALUE 'PAGE '.                
090002     03   HRAD1-IDSIDNR           PIC Z(2)9 VALUE ZERO.                   
100002                                                                          
110002 01  LIST-HRAD2.                                                          
120099     03   FILLER                  PIC X(1)  VALUE SPACE.                  
130099     03   FILLER                  PIC X(15) VALUE                         
140099                             'SHIPPING NO:   '.                           
150099     03   HRAD2-IDRETSND          PIC X(6)  VALUE SPACE.                  
160002                                                                          
170099 01  LIST-HRAD3.                                                          
180099     03   FILLER                  PIC X(31) VALUE SPACE.                  
190099     03   FILLER                  PIC X(11) VALUE 'DISCREPANCY'.          
200099                                                                          
210099 01  LIST-HRAD4.                                                          
220099     03   FILLER                  PIC X(5)  VALUE SPACE.                  
230099     03   FILLER                  PIC X(8)  VALUE 'DISTRICT'.             
240099     03   FILLER                  PIC X(5)  VALUE SPACE.                  
250099     03   FILLER                  PIC X(8)  VALUE 'CUSTOMER'.             
260099     03   FILLER                  PIC X(5)  VALUE SPACE.                  
270099     03   FILLER                  PIC X(10) VALUE 'REPORT NO.'.           
280099                                                                          
290099 01  LIST-KRAD.                                                           
300099     03   FILLER                  PIC X(1)  VALUE SPACE.                  
310099     03   FILLER                  PIC X(5)  VALUE 'CASE '.                
320099     03   KRAD-IDKOLLI            PIC Z(5)  VALUE ZERO.                   
330002                                                                          
340003 01  LIST-LRAD.                                                           
350099     03   FILLER                  PIC X(8)  VALUE SPACE.                  
360099     03   LRAD-IDDISTR            PIC Z(4)9.                              
370099     03   FILLER                  PIC X(7)  VALUE SPACE.                  
380099     03   LRAD-IDKUNDNR           PIC Z(5)9.                              
390099     03   FILLER                  PIC X(5)  VALUE SPACE.                  
400099     03   LRAD-IDRAPPNR           PIC X(7).                               
410002                                                                          
411099 01  LIST-SLUT-RAD.                                                       
412099     03   FILLER                  PIC X(2)  VALUE SPACE.                  
413099     03   FILLER                  PIC X(8)  VALUE 'NO CASES'.             
414099     03   FILLER                  PIC X(4)  VALUE SPACE.                  
415099     03   SLUT-KVKOLLI            PIC Z(2)9.                              
418099                                                                          
420000 LINKAGE SECTION.                                                         
430000                                                                          
440099*01  -COPY W4074310                                                       
450099     EJECT                                                                
460003*01  -COPY W0009   -PRE ALT-                                              
470003     EJECT                                                                
480099*01  -COPY W0008   -PRE RETA-                                             
490003     05  FILLER                  PIC X.                                   
490199     EJECT                                                                
491099*01  -COPY W0008   -PRE LISB-                                             
492099     05  FILLER                  PIC X.                                   
500003     EJECT                                                                
510099 PROCEDURE DIVISION  USING W407-W4074310                                  
520099                           ALT-PCB RETA-PCB LISB-PCB.                     
530099                                                                          
540099     PERFORM A-INIT                                                       
550099     PERFORM B-SKAPA-SND-UTSKRIFT                                         
560002                                                                          
570007     PERFORM Z-FINIT                                                      
580000     MOVE ZERO TO RETURN-CODE                                             
590000     GOBACK                                                               
600000     .                                                                    
610000     EJECT                                                                
620000 A-INIT SECTION.                                                          
630000                                                                          
640099     MOVE LOW-VALUE              TO W-WDA3BSEQ-MIN-X                      
650099     MOVE HIGH-VALUE             TO W-WDA3BSEQ-MAX-X                      
660099                                                                          
661099     IF W407-IDRT = 'GB1' OR 'GB3'                                        
661199        MOVE 3                   TO PRT-COPIES-OVR                        
662099     ELSE                                                                 
662199        MOVE 1                   TO PRT-COPIES-OVR                        
663099     END-IF                                                               
670099     MOVE ZERO                   TO W-SPAR-IDKOLLI                        
680085                                                                          
690002     PERFORM AA-OPEN-PRINTER                                              
700000     .                                                                    
710000     EJECT                                                                
720002 AA-OPEN-PRINTER         SECTION.                                         
730002                                                                          
740099     MOVE W407-IDPRTLST       TO WS-RAPP-PRINTER                          
741099     MOVE W407-IDRTLOP        TO WS-PRT-LOPNR                             
801099     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
802099                         PRT-OPEN                                         
802199                         WS-RAPP-PRINTER                                  
804099                         ALT-PCB                                          
805099                         LISB-PCB                                         
806099                         WS-PRT-IDLIST                                    
807099                         WS-DUMMY                                         
808099                         WS-DUMMY                                         
810002     .                                                                    
820002     EJECT                                                                
830099 B-SKAPA-SND-UTSKRIFT    SECTION.                                         
840002                                                                          
850099     MOVE ZERO             TO W-IDSIDNR                                   
860099                                                                          
870099     MOVE W407-IDDC        TO W-IDDC-BSEQ-MIN                             
880099                              W-IDDC-BSEQ-MAX                             
881099     MOVE W407-IDRT        TO W-IDRT-BSEQ-MIN                             
882099                              W-IDRT-BSEQ-MAX                             
890099     MOVE W407-IDRTLOP     TO W-IDRTLOP-BSEQ-MIN                          
900099                              W-IDRTLOP-BSEQ-MAX                          
910081                                                                          
920099     PERFORM IMS-GU-WLRETA01                                              
930099     PERFORM S01-SKAPA-HUVUD                                              
940002                                                                          
950099     PERFORM UNTIL SEGMENT-SAKNAS                                         
960099        PERFORM BB-SKAPA-LIST-RADER                                       
970099        PERFORM IMS-GN-WLRETA01                                           
980099     END-PERFORM                                                          
980199     MOVE PRT-AFTER-3    TO PRT-RADSKIP                                   
981099     MOVE W-KVKOLLI      TO SLUT-KVKOLLI                                  
982099     MOVE LIST-SLUT-RAD  TO WS-RAPP-RAD                                   
983099     PERFORM S02-SKRIV-RAD                                                
990099     .                                                                    
000002     EJECT                                                                
010099                                                                          
020099 BB-SKAPA-LIST-RADER  SECTION.                                            
030081                                                                          
040099     IF RET-IDKOLLI            =  W-SPAR-IDKOLLI                          
051099        MOVE PRT-AFTER-1       TO PRT-RADSKIP                             
060099     ELSE                                                                 
060199        MOVE RET-IDKOLLI       TO W-SPAR-IDKOLLI                          
061099        ADD +1               TO W-KVKOLLI                                 
070099        PERFORM BBA-SKAPA-KOLLI-HUVUD                                     
080099     END-IF                                                               
090099                                                                          
100099     MOVE RET-IDDISTR          TO LRAD-IDDISTR                            
110099     MOVE RET-IDKUNDNR         TO LRAD-IDKUNDNR                           
120099     MOVE RET-IDRAPPNR         TO LRAD-IDRAPPNR                           
130099                                                                          
140099     IF W-KVRADER              >  MAX-KVRADER                             
150083         PERFORM S01-SKAPA-HUVUD                                          
160083         MOVE PRT-AFTER-1      TO PRT-RADSKIP                             
170083         ADD +1                TO W-KVRADER                               
180083      ELSE                                                                
200099         ADD +1                TO W-KVRADER                               
210083     END-IF                                                               
220083     MOVE LIST-LRAD            TO WS-RAPP-RAD                             
230084     PERFORM S02-SKRIV-RAD                                                
240082     .                                                                    
250082     EJECT                                                                
260082                                                                          
270099 BBA-SKAPA-KOLLI-HUVUD SECTION.                                           
280099                                                                          
290099     MOVE RET-IDKOLLI          TO KRAD-IDKOLLI                            
300099                                                                          
310099     MOVE LIST-KRAD            TO WS-RAPP-RAD                             
320099     PERFORM S02-SKRIV-RAD                                                
320199                                                                          
321099     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
322099     MOVE LIST-HRAD3           TO WS-RAPP-RAD                             
323099     PERFORM S02-SKRIV-RAD                                                
324099                                                                          
325099     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
326099     MOVE LIST-HRAD4           TO WS-RAPP-RAD                             
327099     PERFORM S02-SKRIV-RAD                                                
328099                                                                          
329099     ADD +4                    TO W-KVRADER                               
329199     MOVE PRT-AFTER-2          TO PRT-RADSKIP                             
330099     .                                                                    
340099     EJECT                                                                
350099                                                                          
360082                                                                          
370007 Z-FINIT                   SECTION.                                       
380002                                                                          
390099     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
400002                         PRT-CLOSE                                        
410002                         WS-RAPP-PRINTER                                  
420002                         ALT-PCB                                          
421099                         LISB-PCB                                         
422099                         WS-PRT-IDLIST                                    
430002                         WS-DUMMY                                         
440002                         WS-DUMMY                                         
450002     .                                                                    
460002     EJECT                                                                
470081 S01-SKAPA-HUVUD     SECTION.                                             
480081                                                                          
490081     COMPUTE W-IDSIDNR            =  W-IDSIDNR + 1                        
500081     MOVE W-IDSIDNR               TO HRAD1-IDSIDNR                        
510081     ACCEPT HRAD1-DATUM           FROM DATE                               
520081                                                                          
530081     MOVE PRT-NYSIDA-RAD3         TO PRT-RADSKIP                          
540081     MOVE LIST-HRAD1              TO WS-RAPP-RAD                          
550081     PERFORM S02-SKRIV-RAD                                                
560081                                                                          
570099     MOVE RET-IDRETSND            TO HRAD2-IDRETSND                       
580099                                                                          
590081     MOVE PRT-AFTER-2             TO PRT-RADSKIP                          
600081     MOVE LIST-HRAD2              TO WS-RAPP-RAD                          
610082     PERFORM S02-SKRIV-RAD                                                
620081                                                                          
710081                                                                          
720099     MOVE +8                      TO W-KVRADER                            
730081     .                                                                    
740081     EJECT                                                                
750081 S02-SKRIV-RAD SECTION.                                                   
760002                                                                          
770099     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
780002                         PRT-WRITE                                        
790002                         WS-RAPP-PRINTER                                  
800081                         ALT-PCB                                          
801099                         LISB-PCB                                         
802099                         WS-PRT-IDLIST                                    
810002                         PRT-RADSKIP                                      
820002                         WS-RAPP-LISTRAD                                  
830002                                                                          
840002     MOVE SPACE                TO WS-RAPP-LISTRAD                         
850002     .                                                                    
860002     EJECT                                                                
870000                                                                          
880000* --- IMS SEKTIONER ---                                                   
890000     SKIP3                                                                
900099 IMS-GU-WLRETA01    SECTION.                                              
910081                                                                          
920099     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
930099                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
940029          DELIMITED BY SIZE INTO SSA1                                     
950029     MOVE '  GE' TO GODK-STATUSKODER                                      
960099     CALL CBLTDLI USING GU RETA-PCB DLI-IO-AREA1 SSA1                     
970099     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
980029     PERFORM IMS-STATUSKONTROLL                                           
990029     .                                                                    
000029     SKIP2                                                                
010099 IMS-GN-WLRETA01    SECTION.                                              
020081                                                                          
030099     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
040099                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
050081          DELIMITED BY SIZE INTO SSA1                                     
060081     MOVE '  GEGB' TO GODK-STATUSKODER                                    
070099     CALL CBLTDLI USING GN RETA-PCB DLI-IO-AREA1 SSA1                     
080099     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
090081     PERFORM IMS-STATUSKONTROLL                                           
100081     .                                                                    
110081     SKIP2                                                                
120000 IMS-STATUSKONTROLL SECTION.                                              
130000                                                                          
140000     SET STATUS-IX TO 1                                                   
150000     SEARCH GODK-STATUS                                                   
160000       AT END                                                             
170000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
180000         DELIMITED BY SIZE INTO FELTEXT                                   
190000         CALL FELLOG                                                      
200000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
210000         CONTINUE                                                         
220000     END-SEARCH                                                           
230000     .                                                                    
