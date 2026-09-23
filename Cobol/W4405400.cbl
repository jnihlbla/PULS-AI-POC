000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4405400.                                    
000300 AUTHOR.                     MÅNS SAMUELSSON.                             
000400 DATE-WRITTEN.               SEPT 1988.                                   
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*             *SB*                                                        
001000*    LÄSER NER WDK611 TILL EN SEKV. FIL                                   
001100*                                                                         
001200*    UTFIL: W44054                                                        
001300*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700 FILE-CONTROL.                                                            
001800                                                                          
001900     SELECT W44054           ASSIGN TO      W44054D1.                     
002000                                                                          
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400     SKIP2                                                                
002500 FD  W44054                                                               
002600     LABEL RECORD STANDARD                                                
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900*01  W44054-POST -COPY W440003    -L                                      
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP2                                                                
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W4405400'.            
003300 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003400                                                                          
003500*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
003600                                                                          
003610*      --- VALID IDDC CODES                                               
003620*                                                                         
003630*01    -COPY WWDCKONS                                                     
003640       EJECT                                                              
003700 01  DYNAMISKA-SUBPROGRAM.                                                
003800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
003900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
004000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
004100     EJECT                                                                
004200*    ---- PARAMETRAR TILL POSTSUM                                         
004300                                                                          
004400*01  -COPY W0005      -PRE POSTSUM-.                                      
004500                                                                          
004600*    ---- SKRIVAREA.                                                      
004700*01  AREA  -COPY W440003    -PRE UT-                                      
004800                                                                          
004900*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
005000                                                                          
005100 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
005200                                                                          
005300*    ---- STATUSKOD FRÅN IMS                                              
005400                                                                          
005500 01  STATUS-WS                   PIC XX.                                  
005600     88  SEGMENT-FINNS                      VALUE '  '.                   
005700     88  SEGMENT-SLUT                       VALUE 'GB'.                   
005800     SKIP3                                                                
005900 01  GODK-STATUSKODER.                                                    
006000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
006100     SKIP3                                                                
006200 01  SSA1                        PIC X(32).                               
006300     EJECT                                                                
006400*01      -COPY W0003.                                                     
006500     EJECT                                                                
006600                                                                          
006700 01  FILLER                       PIC X(16)  VALUE 'DLI-IO-AREA'.         
006800 01  DLI-IO-AREA.                                                         
006900*  03  WLARC11 -COPY WDK611                                               
007000     EJECT                                                                
007100 LINKAGE SECTION.                                                         
007200     SKIP2                                                                
007300*    -COPY W0008 -PRE WDK6-.                                              
007400    05  FB-IDARTNR               PIC S9(9) COMP-3.                        
007500     EJECT                                                                
007600 PROCEDURE DIVISION  USING WDK6-PCB.                                      
007700     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
007800                                                                          
007900     PERFORM A-INIT                                                       
008000                                                                          
008100     PERFORM IMS-GN-WDK6                                                  
008200     PERFORM UNTIL SEGMENT-SLUT                                           
008300                                                                          
008400       IF WDK6-SEG-NAME-FB = 'WDK611  '                                   
008500         PERFORM S01-SKRIV-UTPOST                                         
008600         CALL    POSTSUM USING POSTSUM-PARM                               
008700       END-IF                                                             
008800                                                                          
008900       PERFORM IMS-GN-WDK6                                                
009000     END-PERFORM                                                          
009100                                                                          
009200     PERFORM Z-FINIT                                                      
009300     MOVE    ZERO TO RETURN-CODE                                          
009400                                                                          
009500     GOBACK                                                               
009600     .                                                                    
009700     EJECT                                                                
009800 A-INIT SECTION.                                                          
009900                                                                          
010000     OPEN  OUTPUT W44054                                                  
010100     MOVE  PROGRAM-NAMN  TO POSTSUM-PROGNAMN                              
010200     MOVE 'W44054'       TO POSTSUM-FDNAMN                                
010300     MOVE 'W44054D1'     TO POSTSUM-DDNAMN2                               
010400                                                                          
010500     .                                                                    
010600     EJECT                                                                
010700 Z-FINIT SECTION.                                                         
010800                                                                          
010900     CLOSE W44054                                                         
011000     MOVE 'S'      TO    POSTSUM-OPKOD                                    
011100     CALL  POSTSUM USING POSTSUM-PARM                                     
011200     .                                                                    
011300     EJECT                                                                
011400 S01-SKRIV-UTPOST SECTION.                                                
011500                                                                          
011600     MOVE  '003'        TO   UT-ART-IDPTYP                                
011700     MOVE   FB-IDARTNR  TO   UT-ART-IDARTNR                               
011720     MOVE   WC-CDC-SE   TO   UT-ART-IDDC                                  
011800     MOVE   CLAG-KVROS  TO   UT-ART-KVROS                                 
011900     MOVE   CLAG-KVRESS TO   UT-ART-KVRESS                                
012000     WRITE  W44054-POST FROM UT-AREA                                      
012100     .                                                                    
012200     EJECT                                                                
012300*    ---- IMS SEKTIONER                                                   
012400 IMS-GN-WDK6 SECTION.                                                     
012500                                                                          
012600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
012700     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
012800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
012900     PERFORM IMS-STATUSKONTROLL                                           
013000     .                                                                    
013100     SKIP3                                                                
013200 IMS-STATUSKONTROLL SECTION.                                              
013300                                                                          
013400     SET STATUS-IX TO 1                                                   
013500     SEARCH GODK-STATUS                                                   
013600       AT END                                                             
013700         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
013800         DELIMITED BY SIZE INTO FELTEXT                                   
013900         CALL FELLOG                                                      
014000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
014100         CONTINUE                                                         
014200     END-SEARCH                                                           
014300     .                                                                    
