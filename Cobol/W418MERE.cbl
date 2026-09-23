010000*COMPOPT STDSUB=YES                                                       
020000 ID DIVISION.                                                             
030000 PROGRAM-ID.     W418MERE.                                                
040000 AUTHOR.         LARS THELL CAP PROGRAMATOR                               
050000 DATE-WRITTEN.   JULI 1995.                                               
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*    FUNKTION:                                                            
090000*        PROGRAMMET SKAPAR MAIL TILL REMISSANSVARIG                       
100000*                                                                         
101000*    E'TRACKER 3927424  2006-09-27                                        
101100*    E'TRACKER 8687963  2010-10-29                                        
102000*                                                                         
110000                                                                          
120000 ENVIRONMENT DIVISION.                                                    
130000                                                                          
140000 DATA DIVISION.                                                           
150000                                                                          
160000     EJECT                                                                
170000 WORKING-STORAGE SECTION.                                                 
180000 77  IDPGM                   PIC X(8) VALUE 'W418MERE'.                   
190000 77  JA                      PIC X       VALUE 'J'.                       
200000 77  NEJ                     PIC X       VALUE 'N'.                       
210000 77  INDX                    PIC S9(4)   VALUE ZERO COMP SYNC.            
220000 77  TEXT-INDX               PIC S9(4)   VALUE ZERO COMP SYNC.            
230000                                                                          
240000 01  FILLER.                                                              
250000   03 COUNTER                PIC 99  VALUE ZERO.                          
260000                                                                          
270000 01  DYNAMISKA-SUBPROGRAM.                                                
280000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
290000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
291000   03  W009EMAD              PIC X(8)    VALUE 'W009EMAD'.                
300000                                                                          
310000     EJECT                                                                
320000*  MAILRADER                                                              
330000                                                                          
340000 01  LIST-HRAD1.                                                          
350000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
360000     03   FILLER                  PIC X(12) VALUE                         
370000                                        'W418MERE-001'.                   
380000     03   FILLER                  PIC X(4)  VALUE SPACE.                  
390000     03   FILLER                  PIC X(28)  VALUE                        
400000                                  'REFERRAL OF DISCREPANCY REP.'.         
410000                                                                          
420000 01  LIST-HRAD2.                                                          
430000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
440000     03   FILLER                  PIC X(9)  VALUE 'DISTRICT '.            
450000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
460000     03   HRAD2-IDDISTR           PIC Z(4)  VALUE ZERO.                   
470000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
480000     03   FILLER                  PIC X(5)  VALUE 'CUST '.                
490000     03   HRAD2-IDKUNDNR          PIC Z(6)  VALUE ZERO.                   
500000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
510000     03   FILLER                  PIC X(10) VALUE 'D/R NO.   '.           
520000     03   HRAD2-IDRAPPNR          PIC Z(7)  VALUE ZERO.                   
530000                                                                          
540000 01  LIST-HRAD3.                                                          
550000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
560000     03   FILLER                  PIC X(6)  VALUE 'PARTNO'.               
570000     03   HRAD3-IDARTNR           PIC Z(8)  VALUE ZERO.                   
580000     03   FILLER                  PIC X(1)  VALUE '-'.                    
590000     03   HRAD3-REKSIFFR          PIC 9.                                  
600000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
610000     03   FILLER                  PIC X(6)  VALUE 'LINE  '.               
620000     03   HRAD3-IDRADNR           PIC Z(5)  VALUE ZERO.                   
630000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
640000     03   FILLER                  PIC X(5)  VALUE 'RESP '.                
650000     03   HRAD3-KDARBTYP          PIC X(3)  VALUE SPACE.                  
660000     03   HRAD3-IDPERSON          PIC 9(3)  VALUE ZERO.                   
670000                                                                          
680000 01  LIST-LRAD15.                                                         
690000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
700000     03   FILLER                  PIC X(26) VALUE                         
710000                           'MESSAGE FROM VIPS/PULS:  '.                   
720000                                                                          
730000 01  LIST-LRAD16.                                                         
740000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
750000     03   FILLER                  PIC X(20) VALUE                         
760000                           'MESSAGE FROM ADM:   '.                        
770000                                                                          
780000 01  LIST-LRAD17.                                                         
790000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
800000     03   FILLER                  PIC X(23) VALUE                         
810000                           'MESSAGE FROM REM:      '.                     
820000                                                                          
830000 01  LIST-LRAD18.                                                         
840000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
850000     03   FILLER                  PIC X(23) VALUE                         
860000                           'MESSAGE FROM RET.DEPT:'.                      
870000                                                                          
880000 01  LIST-LRAD19.                                                         
890000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
900000     03   FILLER                  PIC X(54) VALUE                         
910000         '*USE IMSVCP PICTURE 4723 TO GIVE COMMENTS            *'.        
920000                                                                          
930000 01  LIST-LRAD20.                                                         
940000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
950000     03   FILLER                  PIC X(54) VALUE                         
960000         '*USE IMSVCP PICTURE 4722 TO GIVE REFFERALANSWERS Y/N.*'.        
970000                                                                          
980000 01  LIST-LRAD-TEAMNOT.                                                   
990000     03   FILLER                  PIC X(1) VALUE SPACE.                   
000000     03   LRAD-TEANMNOT           PIC X(70).                              
010000                                                                          
020000 01  LIST-BLANKRAD.                                                       
030000     03   FILLER                  PIC X(80) VALUE SPACE.                  
040000                                                                          
050000 01  LIST-STRECKRAD.                                                      
060000     03   FILLER                  PIC X(1) VALUE SPACE.                   
070000     03   FILLER                  PIC X(70) VALUE ALL '='.                
080000                                                                          
090000 01  LIST-ASTERRAD.                                                       
100000     03   FILLER                  PIC X(1) VALUE SPACE.                   
110000     03   FILLER                  PIC X(54) VALUE ALL '*'.                
120000                                                                          
130000     EJECT                                                                
140000*01  -COPY WMSGAREA                                                       
150000*   --- PARAMETRAR TILL PROGRAM W0541X                                    
160000                                                                          
170000 01  FILLER                     PIC X(16)   VALUE 'WMSGMAIL-AREA'.        
180000*01  -COPY WMSGMAIL                                                       
190000                                                                          
191000                                                                          
192000     EJECT                                                                
194000*   --- PARAMETRAR TILL PROGRAM W009EMAD                                  
195000                                                                          
196000 01  FILLER                     PIC X(16)   VALUE 'W009EMAD-AREA'.        
197000*01  -COPY W009EMAD                                                       
198000                                                                          
200000     EJECT                                                                
210000*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
220000                                                                          
230000 01  GODK-STATUSKODER.                                                    
240000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
250000                                                                          
260000 01  SSA1                    PIC X(32).                                   
270000                                                                          
280000*    --- STATUS-KOD FRÅN IMS                                              
290000 01  STATUS-WS                   PIC XX.                                  
300000     88  STATUS-OK                           VALUE '  '.                  
310000                                                                          
320000*    --- IMS FUNKTIONSKODER                                               
330000*01  -COPY W0003                                                          
340000                                                                          
350000     EJECT                                                                
360000 LINKAGE SECTION.                                                         
370000*01  -COPY W418MERE                                                       
380000                                                                          
390000     EJECT                                                                
400000*01  -COPY W0009      -PRE  MAIL-                                         
410000                                                                          
420000     EJECT                                                                
430000 PROCEDURE DIVISION  USING  MERE-W418MERE MAIL-PCB.                       
440000                                                                          
450000 STYR SECTION.                                                            
460000                                                                          
470000     PERFORM A-INIT                                                       
480000                                                                          
490000     PERFORM B-SKAPA-MAIL                                                 
500000                                                                          
510000     GOBACK                                                               
520000     .                                                                    
530000                                                                          
540000     EJECT                                                                
550000                                                                          
560000 A-INIT        SECTION.                                                   
570000     MOVE '4722'                TO MAIL-IDTRANS                           
580000     MOVE '1'                   TO MAIL-KDMFSFOR                          
590000     MOVE 'REMISS  '            TO MAIL-IDMAILTTL                         
600000     MOVE +1                    TO INDX                                   
610000     MOVE LIST-HRAD1            TO MAIL-TEMAIL (INDX)                     
620000     ADD +1                     TO INDX                                   
630000                                                                          
640000*    MOVE ZERO TO COUNTER                                                 
650000*    INSPECT MERE-IDMAIL TALLYING COUNTER FOR ALL '@'                     
651000     MOVE MERE-IDMAIL TO EMAD-IDMAIL                                      
652000     CALL W009EMAD USING EMAD-W009EMAD                                    
660000     IF EMAD-KDSVAR = 'F'                                                 
670000       MOVE 'SSAMUEL2@VOLVOCARS.COM'                                      
680000                                TO MAIL-IDMAIL                            
690000     ELSE                                                                 
700000       MOVE MERE-IDMAIL         TO MAIL-IDMAIL                            
710000     END-IF                                                               
720000     .                                                                    
730000                                                                          
740000     EJECT                                                                
750000 B-SKAPA-MAIL  SECTION.                                                   
760000                                                                          
770000     PERFORM BA-REDIGERA-HUVUD                                            
780000     PERFORM BB-REDIGERA-RADER                                            
790000                                                                          
800000     MOVE INDX                  TO MAIL-KVMAILLN                          
810000                                                                          
830000     PERFORM IMS-INSERT-TRANS0541X-MID                                    
840000     .                                                                    
850000                                                                          
860000     EJECT                                                                
870000 BA-REDIGERA-HUVUD  SECTION.                                              
880000                                                                          
890000     MOVE MERE-IDDISTR          TO HRAD2-IDDISTR                          
900000     MOVE MERE-IDKUNDNR         TO HRAD2-IDKUNDNR                         
910000     MOVE MERE-IDRAPPNR         TO HRAD2-IDRAPPNR                         
920000     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
930000     ADD +1                     TO INDX                                   
940000     MOVE LIST-HRAD2            TO MAIL-TEMAIL (INDX)                     
950000                                                                          
960000     ADD +1                     TO INDX                                   
970000     MOVE MERE-IDARTNR          TO HRAD3-IDARTNR                          
980000     MOVE MERE-REKSIFFR         TO HRAD3-REKSIFFR                         
990000     MOVE MERE-IDRADNR          TO HRAD3-IDRADNR                          
000000     MOVE MERE-KDARBTYP(1:3)    TO HRAD3-KDARBTYP                         
010000     MOVE MERE-IDPERSON         TO HRAD3-IDPERSON                         
020000     MOVE LIST-HRAD3            TO MAIL-TEMAIL (INDX)                     
030000     ADD +1                     TO INDX                                   
040000     .                                                                    
050000                                                                          
060000     EJECT                                                                
070000 BB-REDIGERA-RADER  SECTION.                                              
080000                                                                          
090000     ADD +1                     TO INDX                                   
100000     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
110000                                                                          
120000     ADD +1                     TO INDX                                   
130000     PERFORM BBA-FIXA-TEXT-INFO                                           
140000                                                                          
150000     MOVE LIST-ASTERRAD         TO MAIL-TEMAIL (INDX)                     
160000     ADD +1                     TO INDX                                   
170000                                                                          
180000     MOVE LIST-LRAD19           TO MAIL-TEMAIL (INDX)                     
190000     ADD +1                     TO INDX                                   
200000                                                                          
210000     MOVE LIST-LRAD20           TO MAIL-TEMAIL (INDX)                     
220000     ADD +1                     TO INDX                                   
230000                                                                          
240000     MOVE LIST-ASTERRAD         TO MAIL-TEMAIL (INDX)                     
250000     ADD +1                     TO INDX                                   
260000     .                                                                    
270000                                                                          
280000     EJECT                                                                
290000 BBA-FIXA-TEXT-INFO  SECTION.                                             
300000                                                                          
310000     PERFORM BBAA-TEANMNOT-REG                                            
320000     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
330000     ADD +1                     TO INDX                                   
340000                                                                          
350000     PERFORM BBAB-TEANMNOT-ADM                                            
360000     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
370000     ADD +1                     TO INDX                                   
380000                                                                          
390000     PERFORM BBAC-TEANMNOT-REM                                            
400000     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
410000     ADD +1                     TO INDX                                   
420000                                                                          
430000     PERFORM BBAD-TEANMNOT-RET                                            
440000     .                                                                    
450000                                                                          
460000     EJECT                                                                
470000 BBAA-TEANMNOT-REG SECTION.                                               
480000                                                                          
490000     MOVE LIST-LRAD15           TO MAIL-TEMAIL (INDX)                     
500000     ADD +1                     TO INDX                                   
510000                                                                          
520000     MOVE LIST-STRECKRAD        TO MAIL-TEMAIL (INDX)                     
530000     ADD +1                     TO INDX                                   
540000                                                                          
550000     MOVE +1                    TO TEXT-INDX                              
560000     PERFORM UNTIL TEXT-INDX    >  3                                      
570000        MOVE MERE-TEANMNOT-REG (TEXT-INDX)                                
580000                                TO LRAD-TEANMNOT                          
590000        MOVE LIST-LRAD-TEAMNOT  TO MAIL-TEMAIL (INDX)                     
600000        ADD +1                  TO INDX                                   
610000                                   TEXT-INDX                              
620000     END-PERFORM                                                          
630000     .                                                                    
640000                                                                          
650000     EJECT                                                                
660000 BBAB-TEANMNOT-ADM SECTION.                                               
670000                                                                          
680000     MOVE LIST-LRAD16           TO MAIL-TEMAIL (INDX)                     
690000     ADD +1                     TO INDX                                   
700000                                                                          
710000     MOVE LIST-STRECKRAD        TO MAIL-TEMAIL (INDX)                     
720000     ADD +1                     TO INDX                                   
730000                                                                          
740000     MOVE +1                    TO TEXT-INDX                              
750000     PERFORM UNTIL TEXT-INDX    >  3                                      
760000        MOVE MERE-TEANMNOT-ADM (TEXT-INDX)                                
770000                                TO LRAD-TEANMNOT                          
780000        MOVE LIST-LRAD-TEAMNOT  TO MAIL-TEMAIL (INDX)                     
790000        ADD +1                  TO INDX                                   
800000                                   TEXT-INDX                              
810000     END-PERFORM                                                          
820000     .                                                                    
830000                                                                          
840000     EJECT                                                                
850000 BBAC-TEANMNOT-REM SECTION.                                               
860000                                                                          
870000     MOVE LIST-LRAD17           TO MAIL-TEMAIL (INDX)                     
880000     ADD +1                     TO INDX                                   
890000                                                                          
900000     MOVE LIST-STRECKRAD        TO MAIL-TEMAIL (INDX)                     
910000     ADD +1                     TO INDX                                   
920000                                                                          
930000     MOVE +1                    TO TEXT-INDX                              
940000     PERFORM UNTIL TEXT-INDX    >  3                                      
950000        MOVE MERE-TEANMNOT-REM (TEXT-INDX)                                
960000                                TO LRAD-TEANMNOT                          
970000        MOVE LIST-LRAD-TEAMNOT  TO MAIL-TEMAIL (INDX)                     
980000        ADD +1                  TO INDX                                   
990000                                   TEXT-INDX                              
000000     END-PERFORM                                                          
010000     .                                                                    
020000                                                                          
030000     EJECT                                                                
040000 BBAD-TEANMNOT-RET SECTION.                                               
050000                                                                          
060000     MOVE LIST-LRAD18           TO MAIL-TEMAIL (INDX)                     
070000     ADD +1                     TO INDX                                   
080000                                                                          
090000     MOVE LIST-STRECKRAD        TO MAIL-TEMAIL (INDX)                     
100000     ADD +1                     TO INDX                                   
110000                                                                          
120000     MOVE +1                    TO TEXT-INDX                              
130000     PERFORM UNTIL TEXT-INDX    >  3                                      
140000        MOVE MERE-TEANMNOT-RET (TEXT-INDX)                                
150000                                TO LRAD-TEANMNOT                          
160000        MOVE LIST-LRAD-TEAMNOT  TO MAIL-TEMAIL (INDX)                     
170000        ADD +1                  TO INDX                                   
180000                                   TEXT-INDX                              
190000     END-PERFORM                                                          
200000     .                                                                    
210000                                                                          
220000     EJECT                                                                
230000 IMS-INSERT-TRANS0541X-MID SECTION.                                       
240000                                                                          
250000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
260000     MOVE '  '                  TO GODK-STATUSKODER                       
270000     CALL CBLTDLI USING ISRT MAIL-PCB MAIL-WMSGMAIL                       
280000     MOVE MAIL-STATUS-CODE       TO STATUS-WS                             
290000     PERFORM IMS-STATUSKONTROLL                                           
300000     .                                                                    
310000                                                                          
320000                                                                          
330000 IMS-STATUSKONTROLL SECTION.                                              
340000                                                                          
350000     SET STATUS-IX TO 1                                                   
360000     SEARCH GODK-STATUS                                                   
370000       AT END CALL FELLOG                                                 
380000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
390000     END-SEARCH                                                           
400000     .                                                                    
