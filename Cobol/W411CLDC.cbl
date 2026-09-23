001000*COMPOPT STDSUB=YES                                                       
010000 ID DIVISION.                                                             
020000     SKIP2                                                                
030000 PROGRAM-ID.     W411CLDC.                                                
040000 AUTHOR.         GÖRAN KJELLSON.                                          
050000 DATE-WRITTEN.   2011-08-25                                               
060000                                                                          
070000     REMARKS.                                                             
080000*                                                                         
090000*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
100000*                                                                         
110000*    FUNKTION.                                                            
110100*      - PROGRAMMAT HÄMTAR WDB601-SEGMENTET FÖR DC11 OCH                  
110200*        SAMTLIGA DC I INAREAN CLDC-IDDC-CLEAR-GRP                        
110500*                                                                         
190000*                                                                         
200000*        PROGRAMMET LÄSER  WDB6 DC STYRREGISTER                           
210000*                                                                         
220000*        LÄNKAREA: W411CLDC                                               
230000                                                                          
240000     SKIP3                                                                
250000 ENVIRONMENT DIVISION.                                                    
260000     EJECT                                                                
270000 DATA DIVISION.                                                           
280000 WORKING-STORAGE SECTION.                                                 
280100                                                                          
290000 77  IDPGM                       PIC X(08)   VALUE 'W411CLDC'.            
300000 77  JA                          PIC X       VALUE 'J'.                   
310000 77  NEJ                         PIC X       VALUE 'N'.                   
320000 77  WS-IDDC-IX                  PIC S9(9)  VALUE ZERO  COMP SYNC.        
320100 77  IX-DCCLEAR-MAX              PIC S9(9)  VALUE +99   COMP SYNC.        
330000 77  WS-CLDC-IX                  PIC S9(9)  VALUE ZERO  COMP SYNC.        
330100 77  CLDC-IX-DCCLEAR-MAX         PIC S9(9)  VALUE +99   COMP SYNC.        
347000                                                                          
347101*01  -COPY WWDCKONS                                                       
347200                                                                          
348000                                                                          
350000 01  GENERELLA-SUBPROGRAM.                                                
360000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
370000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
380000                                                                          
390000                                                                          
420000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
430000                                                                          
430100 01  NYCKLAR-TILL-DLI.                                                    
430800     03  W-IDDC-B6-X.                                                     
430900         05 W-IDDC-B6                  PIC X(2).                          
431000                                                                          
440000*    --- STATUS-KOD FRÅN IMS                                              
450000 01  STATUS-WS                   PIC XX.                                  
460000     88  SEGMENT-FINNS                       VALUE '  '.                  
470000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
480000                                                                          
490000 01  GODK-STATUSKODER.                                                    
500000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
510000     SKIP3                                                                
520000 01  SSA1                        PIC X(96).                               
530000 01  SSA2                        PIC X(96).                               
540000                                                                          
541000                                                                          
550000*    --- IMS FUNKTIONSKODER                                               
560000*01  -COPY W0003                                                          
580000     EJECT                                                                
730000*    ---  DLI INPUT-OUTPUT AREA                                           
740000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
750000     SKIP3                                                                
790000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
800000 01   DLI-IO-AREA-B601.                                                   
801000*     03  -COPY WDB601                                                    
810000                                                                          
820000                                                                          
830000 LINKAGE SECTION.                                                         
840000*   -COPY W411CLDC                                                        
880000                                                                          
921000*01  -COPY W0008      -PRE WDB6-                                          
922000     05  FILLER                  PIC X.                                   
923000                                                                          
924000                                                                          
925000                                                                          
930000 PROCEDURE DIVISION  USING CLDC-W411CLDC WDB6-PCB.                        
940000                                                                          
940400     MOVE 1                 TO WS-IDDC-IX                                 
940500     MOVE 1                 TO WS-CLDC-IX                                 
940600                                                                          
940700*    FÖRST BLANKAR VI UT SVARSTABELLEN                                    
940800     PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX                            
940900        MOVE SPACE          TO CLDC-WDB601(WS-CLDC-IX)                    
941000        ADD 1               TO WS-CLDC-IX                                 
941100     END-PERFORM                                                          
941200                                                                          
941300*    DC11 SKALL ALLTID LÄSAS IN                                           
941501     MOVE 1                 TO WS-CLDC-IX                                 
941600     MOVE WC-CDC-SE         TO W-IDDC-B6                                  
941700     PERFORM IMS-01-GU-WDB601                                             
941800     IF SEGMENT-FINNS                                                     
941900        MOVE DLI-IO-AREA-B601 TO CLDC-WDB601(WS-CLDC-IX)                  
942000        ADD 1                 TO WS-CLDC-IX                               
942100     END-IF                                                               
942200                                                                          
942300*    SEDAN LÄSER VI IN ALLA SEGMENT FÖR DC I CLEARINGTABELLEN             
942400     PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX      OR                    
942410                   WS-IDDC-IX > IX-DCCLEAR-MAX      OR                    
942500                   CLDC-IDDC-CLEAR(WS-IDDC-IX) = SPACE                    
942600                                                                          
942700        IF CLDC-IDDC-CLEAR(WS-IDDC-IX) NOT = WC-CDC-SE                    
942800           MOVE CLDC-IDDC-CLEAR(WS-IDDC-IX)                               
942900                              TO W-IDDC-B6                                
943000           PERFORM IMS-01-GU-WDB601                                       
943100           IF SEGMENT-FINNS                                               
943200              MOVE DLI-IO-AREA-B601                                       
943300                              TO CLDC-WDB601(WS-CLDC-IX)                  
943400              ADD 1           TO WS-CLDC-IX                               
943500           END-IF                                                         
943600        END-IF                                                            
943702        ADD 1                 TO WS-IDDC-IX                               
943800                                                                          
944000     END-PERFORM                                                          
232000                                                                          
233000     GOBACK                                                               
240000     .                                                                    
250000                                                                          
060000                                                                          
202000 IMS-01-GU-WDB601    SECTION.                                             
203000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
204000          DELIMITED BY SIZE INTO SSA1                                     
205000     MOVE '  GE'              TO GODK-STATUSKODER                         
206000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
207000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
208000     PERFORM IMS-STATUSKONTROLL                                           
209000     .                                                                    
210000                                                                          
220000 IMS-STATUSKONTROLL SECTION.                                              
230000                                                                          
240000     SET STATUS-IX TO 1                                                   
250000     SEARCH GODK-STATUS AT END CALL FELLOG                                
260000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
270000     END-SEARCH                                                           
280000     .                                                                    
