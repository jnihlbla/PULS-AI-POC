000010 PROCESS DYNAM                                                            
000020*   -THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM                   
000030 ID DIVISION.                                                             
000040 PROGRAM-ID.     W5226100.                                                
000050 AUTHOR.         SARASWATHY S.                                            
000060 DATE-WRITTEN.   17/06/15.                                                
000070 DATE-COMPILED.                                                           
000080                                                                          
000090*                                                                         
000091*    FUNCTION:                                                            
000092*        CREATES DAILY VAT FILE                                           
000093*        SELECTS DATA FROM DB2-TABLE T01IVW AND                           
000094*        CREATES A SEQUENCE-FILE FOR V.A.T.                               
000095*                                                                         
000096*    ABENDCODES:                                                          
000097*        U0016 -  . . . .                                                 
000098*        U1000 -  . . . .                                                 
000099*                                                                         
000100                                                                          
000101     SKIP3                                                                
000102 ENVIRONMENT DIVISION.                                                    
000103     SKIP2                                                                
000104 INPUT-OUTPUT SECTION.                                                    
000105                                                                          
000106 FILE-CONTROL.                                                            
000107     SKIP2                                                                
000108*          --- SEQUENCEFILE V.A.T.                                        
000109     SELECT W52261                     ASSIGN TO W52261D1.                
000110     EJECT                                                                
000111 DATA DIVISION.                                                           
000112     SKIP3                                                                
000113 FILE SECTION.                                                            
000114 FD  W52261                                                               
000115     RECORDING       F                                                    
000116     BLOCK CONTAINS  0.                                                   
000117                                                                          
000118*01  POST    -COPY W522VAT    -L.                                         
000119     EJECT                                                                
000120 WORKING-STORAGE SECTION.                                                 
000121                                                                          
000122 77  IDPGM                       PIC X(8)    VALUE 'W5226100'.            
000123*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND                
000124 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
000125                                                                          
000126 77  YES                         PIC X       VALUE 'J'.                   
000127 77  NOO                         PIC X       VALUE 'N'.                   
000128                                                                          
000129 77  KEYS-SW                     PIC X       VALUE 'J'.                   
000130     88  KEYS-OK                             VALUE 'J'.                   
000131     88  KEYS-WRONG                          VALUE 'N'.                   
000132     EJECT                                                                
000133                                                                          
000134*    --- WS-AREA FOR SEQUENCEFILE                                         
000135 01  WS-TODAY                    PIC X(8)    VALUE SPACE.                 
000136 01  WS-YESTERDAY                PIC X(8)    VALUE SPACE.                 
000137 01  WS-IDPTYP                   PIC X(3)    VALUE 'VAT'.                 
000138*                                                                         
000139*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
000140 01  GENERAL-SUBPROGRAMS.                                                 
000141     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000142     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
000143     SKIP3                                                                
000144*    --- PARAMETERS TO ABEND                                              
000145                                                                          
000146 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000147 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000148 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000149 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
000150     SKIP3                                                                
000151 01  MESSAGE-CODES.                                                       
000152     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
000153     EJECT                                                                
000154*                                                                         
000155*    --- PARAMETRAR TILL SUBPROGRAM WZ20DAYS "ADDERA DAGAR DATUM"         
000156 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
000157*01 -COPY WZ20DAYS                                                        
000158     EJECT                                                                
000159*                                                                         
000160 01  VAT-AREA                    PIC X(24)   VALUE 'VAT-AREA'.            
000161                                                                          
000162*01  -COPY W522VAT -PRE VAT-                                              
000163     EJECT                                                                
000164                                                                          
000165 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
000166       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
000167                                                                          
000168 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
000169 01  DB2-WS.                                                              
000170     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
000171         88  CURSOR-OK                       VALUE 000.                   
000172         88  LINES-FOUND                     VALUE 000.                   
000173         88  LINES-MISSING                   VALUE 100.                   
000174         88  DOUBLE-LINES                    VALUE 811.                   
000175         88  RESOURCE-WRONG                  VALUE 904.                   
000176     03  GOOD-SQLCODECODES.                                               
000177         05  GOOD-SQLCODE OCCURS 5                                        
000178             INDEXED BY SQLCODE-IX PIC 9(3).                              
000179     EJECT                                                                
000180                                                                          
000181     EJECT                                                                
000182 01  FILLER                      PIC X(16)  VALUE 'T01IVW-AREA'.          
000183                                                                          
000184*01  -COPY T01IVW   -PRE T01IVW-                                          
000185     EJECT                                                                
000186     EXEC SQL INCLUDE T01IVW END-EXEC.                                    
000187     EJECT                                                                
000188                                                                          
000189 LINKAGE SECTION.                                                         
000190 PROCEDURE DIVISION.                                                      
000191     PERFORM A-INIT                                                       
000192     PERFORM B-PROCESS-LINES                                              
000193     PERFORM Z-FINISH                                                     
000194     MOVE ZERO TO RETURN-CODE                                             
000195     GOBACK                                                               
000196     .                                                                    
000197     EJECT                                                                
000198 A-INIT SECTION.                                                          
000199                                                                          
000200     OPEN OUTPUT W52261                                                   
000201                                                                          
000202     MOVE FUNCTION  CURRENT-DATE(1:8)  TO WS-TODAY                        
000203                                                                          
000204     MOVE WS-TODAY     TO DAYS-TIDATE1                                    
000205     MOVE 'YYYYMMDD'   TO DAYS-KDDATFMT1                                  
000206     MOVE 'YYYYMMDD'   TO DAYS-KDDATFMT2                                  
000207     MOVE SPACE        TO DAYS-TIDATE2                                    
000208                          DAYS-IDCALEND                                   
000209     MOVE -1           TO DAYS-KVDAYS                                     
000210                                                                          
000211     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
000212                                                                          
000213     MOVE DAYS-TIDATE2(1:8) TO WS-YESTERDAY                               
000214     INITIALIZE GOOD-SQLCODECODES                                         
000215     .                                                                    
000216     EJECT                                                                
000217 B-PROCESS-LINES  SECTION.                                                
000218                                                                          
000219     PERFORM DB2-DCL-OPEN-CRS                                             
000220     PERFORM DB2-FETCH-CRS                                                
000221     PERFORM UNTIL LINES-MISSING                                          
000222       MOVE T01IVW-IV-DATA TO VAT-W522VAT                                 
000223       PERFORM S11-WRITE-W52261                                           
000224       PERFORM DB2-FETCH-CRS                                              
000225     END-PERFORM                                                          
000226     PERFORM DB2-CLOSE-CRS                                                
000227     .                                                                    
000228     EJECT                                                                
000229 Z-FINISH SECTION.                                                        
000230                                                                          
000231     CLOSE W52261                                                         
000232     SKIP2                                                                
000233     .                                                                    
000234     EJECT                                                                
000235 S11-WRITE-W52261 SECTION.                                                
000236                                                                          
000237     WRITE POST FROM VAT-W522VAT                                          
000238     SKIP2                                                                
000239     .                                                                    
000240     EJECT                                                                
000241 DB2-DCL-OPEN-CRS SECTION.                                                
000242                                                                          
000243     MOVE 000100 TO GOOD-SQLCODECODES                                     
000244                                                                          
000245     EXEC SQL DECLARE T01IVW-CRS CURSOR FOR                               
000246         SELECT DAREGDAT                                                  
000247              , TIREGTID                                                  
000248              , IDLOPNR                                                   
000249              , IDPTYP                                                    
000250              , FLKLAR                                                    
000251              , IV_DATA                                                   
000252              , IV_DATA2                                                  
000253                                                                          
000254         FROM T01IVW                                                      
000255                                                                          
000256         WHERE DAREGDAT = :WS-YESTERDAY                                   
000257           AND IDPTYP   = :WS-IDPTYP                                      
000258                                                                          
000259     END-EXEC                                                             
000260                                                                          
000261     MOVE 000100 TO GOOD-SQLCODECODES                                     
000262                                                                          
000263     EXEC SQL                                                             
000264        OPEN T01IVW-CRS                                                   
000265     END-EXEC                                                             
000266                                                                          
000267     MOVE SQLCODE TO SQLCODE-WS                                           
000268     PERFORM DB2-STATUS-CHECK                                             
000269     .                                                                    
000270     EJECT                                                                
000271 DB2-FETCH-CRS SECTION.                                                   
000272                                                                          
000273     MOVE 000100  TO GOOD-SQLCODECODES                                    
000274     EXEC SQL                                                             
000275         FETCH T01IVW-CRS                                                 
000276         INTO   :T01IVW-DAREGDAT                                          
000277              , :T01IVW-TIREGTID                                          
000278              , :T01IVW-IDLOPNR                                           
000279              , :T01IVW-IDPTYP                                            
000280              , :T01IVW-FLKLAR                                            
000281              , :T01IVW-IV-DATA                                           
000282              , :T01IVW-IV-DATA2                                          
000283     END-EXEC                                                             
000284                                                                          
000285     MOVE SQLCODE TO SQLCODE-WS                                           
000286     PERFORM DB2-STATUS-CHECK                                             
000287     .                                                                    
000288     EJECT                                                                
000289 DB2-CLOSE-CRS SECTION.                                                   
000290                                                                          
000291     EXEC SQL                                                             
000292         CLOSE T01IVW-CRS                                                 
000293     END-EXEC                                                             
000294     .                                                                    
000295     EJECT                                                                
000296 DB2-STATUS-CHECK SECTION.                                                
000297                                                                          
000298     SET SQLCODE-IX TO 1                                                  
000299     SEARCH GOOD-SQLCODE                                                  
000300       AT END                                                             
000301          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
000302          DELIMITED BY SIZE INTO ERROR-TEXT                               
000303          CALL ABEND USING RKOD-ABEND-DB2                                 
000304       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
000305       CONTINUE                                                           
000306     END-SEARCH                                                           
000307     .                                                                    
