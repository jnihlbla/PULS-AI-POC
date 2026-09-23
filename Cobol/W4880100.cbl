000100*COMPOPT VPOSIX=YES                                                       
001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4880100.                                                
001300 AUTHOR.         BOHLIN HÅKAN.                                            
001400 DATE-WRITTEN.   23/05/16.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        INITIAL LOAD PRODUCT MASTER WHICH SHOULD BE SENT TO SYNQ.        
001900*        SENT VIA API FROM SUBPROGRAM W488PRMA.                           
002000*        READ ALL WDK601 SEGMENTS.                                        
002100*                                                                         
002210*        THE PROGRAM READS     WDK6                                       
002300*                                                                         
002400*    ABENDCODES:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W4880100'.            
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
005000     EJECT                                                                
005100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES TODAYS-DATE.                                        
005300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005500     03  TODAYS-DATE-DAY         PIC 9(2).                                
005600     EJECT                                                                
005700 01  GENERAL-SUBPROGRAMS.                                                 
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  W488PRMA                PIC X(8)    VALUE 'W488PRMA'.            
006300     SKIP2                                                                
006400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006500                                                                          
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006900     SKIP2                                                                
007000 01  ERROR-TEXT.                                                          
007100     03  FILLER                  PIC X(11)   VALUE 'ERROR-TEXT '.         
007200     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
007401     EJECT                                                                
007410*01  -COPY W488PRMA                                                       
007600     EJECT                                                                
007700*    --- AREAS FOR IMS-SECTIONS                                           
007800*                                                                         
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  KEYS-FOR-DLI.                                                        
008301     03  W-IDARTNR-X.                                                     
008310         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008400     SKIP2                                                                
008500*    --- STATUS-KOD FRÅN IMS                                              
008600 01  STATUS-WS                   PIC XX.                                  
008700     88  SEGMENT-FOUND                       VALUE '  '.                  
008800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008910     88  SEGMENT-END                         VALUE 'GB'.                  
009000     SKIP2                                                                
009100 01  GOOD-STATUSCODES.                                                    
009200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNCTION CODES                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010202 01  DLI-IO-WDK601.                                                       
010210*    03  -COPY WDK601                                                     
010500     EJECT                                                                
010600 LINKAGE SECTION.                                                         
010700                                                                          
010804     EJECT                                                                
010805*01    -COPY W0008  -PRE WDK6-                                            
010810     05  FILLER               PIC X.                                      
010820     EJECT                                                                
010821 01  SYNQ-ATAB-PCB            PIC X.                                      
010830 01  SYNQ-WDK6-PCB            PIC X.                                      
010840 01  SYNQ-WDD3-PCB            PIC X.                                      
010900     EJECT                                                                
011001 PROCEDURE DIVISION  USING WDK6-PCB SYNQ-ATAB-PCB                         
011002                           SYNQ-WDK6-PCB SYNQ-WDD3-PCB.                   
011003 MAIN SECTION.                                                            
011010     ENTRY 'DLITCBL' USING WDK6-PCB SYNQ-ATAB-PCB                         
011020                           SYNQ-WDK6-PCB SYNQ-WDD3-PCB.                   
011500                                                                          
011600     PERFORM IMS-GN-WDK601                                                
011700     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
011710       MOVE 001         TO SYNQ-KDCALL                                    
011720       MOVE ART-IDARTNR TO SYNQ-IDARTNR                                   
011800       CALL W488PRMA USING  SYNQ-W488PRMA SYNQ-ATAB-PCB                   
011900                            SYNQ-WDK6-PCB SYNQ-WDD3-PCB                   
012400       PERFORM IMS-GN-WDK601                                              
012500     END-PERFORM                                                          
012600                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
015600     EJECT                                                                
015700* --- IMS SECTIONS  ---                                                   
015800                                                                          
015901     EJECT                                                                
015902 IMS-GN-WDK601 SECTION.                                                   
015903                                                                          
015904     MOVE 'WDK601' TO SSA1                                                
015906     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
015907     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK601 SSA1                    
015908     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015909     PERFORM IMS-STATUSCHECK                                              
015910     .                                                                    
016000     EJECT                                                                
016100 IMS-STATUSCHECK SECTION.                                                 
016200                                                                          
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GOOD-STATUS                                                   
016500       AT END                                                             
016600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016700           DELIMITED BY SIZE INTO ERROR-TEXT-STR                          
016800         DISPLAY ERROR-TEXT                                               
016900         CALL FELLOG                                                      
017000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    
