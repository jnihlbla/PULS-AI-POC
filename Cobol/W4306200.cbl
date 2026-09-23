001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4306200.                                                
001300 AUTHOR.         REDDY RAHUL.                                             
001400 DATE-WRITTEN.   11/11/02.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        NEW PROG TO READ WDD3 AND RETRIEVE PART NUMBER AND LANG          
001900*                                                                         
002010*        THE PROGRAM READS     WDD3                                       
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- OUTPUT FILE                                                
003310     SELECT W43062                     ASSIGN TO W43062D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W43062                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  RECORD -COPY W43062 -PRE  UT-  -L.                                   
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4306200'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
004900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES TODAYS-DATE.                                        
005100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005300     03  TODAYS-DATE-DAY         PIC 9(2).                                
005400     EJECT                                                                
       01  WS-LANGUAGE                 PIC X(3).                                
           88  WS-PRINT-LANG           VALUE 'S  ', 'E  ', 'I  ',               
                                             'D  ', 'USA', 'RCN', 'GB '.        
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  ERROR-TEXT.                                                          
006900     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007000     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  UT-AREA-START               PIC X(24)   VALUE                        
007303                                 'UT-AREA-START  '.                       
007304     SKIP2                                                                
007305 01  FILLER       PIC X(80).                                              
007310*01  AREA -COPY W43062     -PRE UT-                                       
007400     EJECT                                                                
007500*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-FOR-DLI.                                                        
008101     03  W-IDARTNR-X.                                                     
008110         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
008101     03  W-IDSKYLT-X.                                                     
               05  W-IDSKYLT-SWEDISH   PIC X(3) VALUE 'S  '.                    
               05  W-IDSKYLT-SPANISH   PIC X(3) VALUE 'E  '.                    
               05  W-IDSKYLT-ITALIAN   PIC X(3) VALUE 'I  '.                    
               05  W-IDSKYLT-GERMAN    PIC X(3) VALUE 'D  '.                    
               05  W-IDSKYLT-AMERICAN  PIC X(3) VALUE 'USA'.                    
               05  W-IDSKYLT-CHINESE   PIC X(3) VALUE 'RCN'.                    
               05  W-IDSKYLT-ENGLISH   PIC X(3) VALUE 'GB '.                    
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008710     88  SEGMENT-END                         VALUE 'GB'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(128).                              
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
010002 01  DLI-IO-WDD311.                                                       
010010*    03  -COPY WDD311  -PRE WDD3-                                         
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010602*01  -COPY W0008  -PRE WDD3-                                              
010610     05  KFB-ARTNR               PIC S9(9)           COMP-3.              
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING WDD3-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDD3-PCB.                                      
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011210     PERFORM IMS-GET-WDD3B1                                               
011300                                                                          
011500     PERFORM UNTIL SEGMENT-END                                            
             MOVE KFB-ARTNR           TO UT-IDARTNR                             
             MOVE WDD3-TEXT-IDSKYLT   TO UT-IDSKYLT WS-LANGUAGE                 
             MOVE WDD3-TEXT-BEART     TO UT-BEART                               
             IF WS-PRINT-LANG                                                   
               PERFORM S11-WRITE-W43062                                         
             END-IF                                                             
011600       PERFORM IMS-GET-WDD3B1                                             
012300     END-PERFORM                                                          
012400                                                                          
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN OUTPUT W43062                                                   
013500                                                                          
013600     ACCEPT TODAYS-DATE  FROM DATE                                        
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000     EJECT                                                                
014100 Z-FINIT SECTION.                                                         
014210     CLOSE W43062                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014601     EJECT                                                                
014602 S11-WRITE-W43062 SECTION.                                                
014603                                                                          
014604     WRITE UT-RECORD FROM UT-AREA                                         
014605                                                                          
014607     MOVE 'W43062' TO POSTSUM-FDNAMN                                      
014608     MOVE 'W43062D1' TO POSTSUM-DDNAMN2                                   
014609     CALL POSTSUM USING POSTSUM-PARM                                      
014610     .                                                                    
014800     EJECT                                                                
014900 S99-ABEND SECTION.                                                       
015000                                                                          
015101     SKIP2                                                                
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND USING RKOD-ABEND                                          
015300     .                                                                    
015400     EJECT                                                                
015500* --- IMS SECTIONS  ---                                                   
015600                                                                          
015701     EJECT                                                                
015702 IMS-GET-WDD3B1 SECTION.                                                  
015703                                                                          
015706*    MOVE 'WDD301   '      TO SSA1                                        
015706     MOVE 'WDD311   '      TO SSA2                                        
015704*    STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-SWEDISH                       
015704*                   '!IDSKYLT  =' W-IDSKYLT-SPANISH                       
015704*                   '!IDSKYLT  =' W-IDSKYLT-ITALIAN                       
015704*                   '!IDSKYLT  =' W-IDSKYLT-GERMAN                        
015704*                   '!IDSKYLT  =' W-IDSKYLT-AMERICAN                      
015704*                   '!IDSKYLT  =' W-IDSKYLT-CHINESE                       
015704*                   '!IDSKYLT  =' W-IDSKYLT-ENGLISH  ')'                  
015705*         DELIMITED BY SIZE INTO SSA2                                     
015708     MOVE '  GB' TO GOOD-STATUSCODES                                      
015709     CALL CBLTDLI USING GN WDD3-PCB DLI-IO-WDD311      SSA2               
015710     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
015713     PERFORM IMS-STATUSCHECK                                              
015720     .                                                                    
015800     EJECT                                                                
015900 IMS-STATUSCHECK SECTION.                                                 
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GOOD-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO ERROR-TEXT                              
016600         DISPLAY ERROR-TEXT                                               
016700         CALL FELLOG                                                      
016800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
