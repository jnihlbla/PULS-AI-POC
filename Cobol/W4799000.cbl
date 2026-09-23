000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4799000.                                                
000400*AUTHOR.         BOO HAMMARIN CGL.                                        
000500*DATE-WRITTEN.   92/04/02.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER WLXXLB OCH SELEKTERAR SAMTLIGA SEGMENT                     
001100*              TILL EN "SB"-FIL                                           
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- UTFIL                                                      
002600     SELECT W47990                     ASSIGN TO W47990D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W47990                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500     SKIP2                                                                
003600 01  W47990-POST.                                                         
003610*03  -COPY W47990    -L.                                                  
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900     SKIP2                                                                
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W4799000'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  W-IDDC-SPAR                 PIC 9(2)    VALUE ZERO.                  
004600                                                                          
004700     EJECT                                                                
004710*      --- VALID IDDC CODES                                               
004720*                                                                         
004730*01    -COPY WWDCKONS                                                     
004740       EJECT                                                              
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900*                                                                         
005000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005400     SKIP2                                                                
005500*    --- PARAMETRAR TILL ABEND                                            
005600                                                                          
005700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL POSTSUM                                          
006500*                                                                         
006600*01  -COPY W0005   -PRE  POSTSUM-                                         
006700     EJECT                                                                
006800 01  UT-AREA-START               PIC X(24)   VALUE                        
006900                                 'UT-AREA-START  '.                       
007000     SKIP2                                                                
007100                                                                          
007200*01  AREA -COPY W47990     -PRE W-                                        
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900*    --- NYCKLAR TILL IMS                                                 
008000 01  W-4477-KEY-X.                                                        
008100     03  W-4477-IDHTYP           PIC X(4)    VALUE '4477'.                
008200     03  W-4477-IDDC             PIC X(2)    VALUE SPACE.                 
008300     03  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
008400     SKIP2                                                                
008500*    --- STATUS-KOD FRÅN IMS                                              
008600 01  STATUS-WS                   PIC XX.                                  
008700     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     88  BASEN-SLUT                          VALUE 'GB'.                  
009000     SKIP2                                                                
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA.                                                         
010400     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010500     03  WLXXLB01                REDEFINES IO-AREA.                       
010600*        05  -COPY WDGX4477                                               
010700     EJECT                                                                
010800     03  WLXXLB11                REDEFINES IO-AREA.                       
010900*        05  -COPY WDGX4478                                               
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200                                                                          
011300     EJECT                                                                
011400*01  -COPY W0008  -PRE XXLB-                                              
011500     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011700 PROCEDURE DIVISION  USING XXLB-PCB.                                      
011800     ENTRY 'DLITCBL' USING XXLB-PCB.                                      
011900                                                                          
012000     PERFORM A-INIT                                                       
012100                                                                          
012200     PERFORM B-LAES-SKRIV-CDC-INFO                                        
012300                                                                          
012400     PERFORM C-LAES-SKRIV-SDC-INFO                                        
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013300                                                                          
013400     OPEN OUTPUT W47990                                                   
013500                                                                          
013600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013800     .                                                                    
013900     EJECT                                                                
014000 B-LAES-SKRIV-CDC-INFO SECTION.                                           
014100                                                                          
014200     MOVE WC-CDC-SE               TO W-4477-IDDC                          
014210     MOVE WC-CDC-SE               TO W-IDDC-SPAR                          
014300                                                                          
014400     PERFORM IMS-GU-XXLB01-CDC                                            
014500                                                                          
014600     PERFORM IMS-GNP-XXLB11                                               
014700                                                                          
014800     PERFORM UNTIL SEGMENT-SAKNAS                                         
014900                                                                          
015100       PERFORM S01-FLYTTA-SKRIV-W47990                                    
015200       PERFORM IMS-GNP-XXLB11                                             
015210                                                                          
015300     END-PERFORM                                                          
015700     .                                                                    
015800     EJECT                                                                
015900 C-LAES-SKRIV-SDC-INFO SECTION.                                           
016000                                                                          
016100     MOVE WC-SDC-NL             TO W-IDDC-SPAR                            
016110     MOVE WC-SDC-NL             TO W-4477-IDDC                            
016200                                                                          
016300     PERFORM IMS-GU-XXLB01-SDC                                            
016400                                                                          
016500     PERFORM UNTIL SEGMENT-SAKNAS                                         
016600       PERFORM IMS-GNP-XXLB11                                             
016700                                                                          
016800       PERFORM UNTIL SEGMENT-SAKNAS                                       
016810                                                                          
017100         PERFORM S01-FLYTTA-SKRIV-W47990                                  
017200         PERFORM IMS-GNP-XXLB11                                           
017201                                                                          
017300       END-PERFORM                                                        
017400                                                                          
017700       ADD  1                     TO W-IDDC-SPAR                          
017710       MOVE W-IDDC-SPAR           TO W-4477-IDDC                          
017800       PERFORM IMS-GU-XXLB01-SDC                                          
017900                                                                          
018200     END-PERFORM                                                          
018210     .                                                                    
018220     EJECT                                                                
018300 Z-FINIT SECTION.                                                         
018400                                                                          
018500     CLOSE W47990                                                         
018600                                                                          
018700     MOVE 'S' TO POSTSUM-OPKOD                                            
018800     CALL POSTSUM USING POSTSUM-PARM                                      
018900     .                                                                    
019000     EJECT                                                                
019100 S01-FLYTTA-SKRIV-W47990 SECTION.                                         
019200                                                                          
019210     MOVE 4478-IDSHIFT   TO W-IDSHIFT                                     
019220     MOVE 4478-IDUSER    TO W-IDUSER                                      
019300     MOVE W-IDDC-SPAR    TO W-IDDC                                        
019400     WRITE W47990-POST FROM W-AREA                                        
019500                                                                          
019600     MOVE 'UT'      TO POSTSUM-TRANSTYP                                   
019700     MOVE 'W47990' TO POSTSUM-FDNAMN                                      
019800     MOVE 'W47990D1' TO POSTSUM-DDNAMN2                                   
019900     CALL POSTSUM USING POSTSUM-PARM                                      
020000                                                                          
020100     .                                                                    
020200     EJECT                                                                
021000 IMS-GU-XXLB01-CDC SECTION.                                               
021100                                                                          
021200     STRING 'WLXXLB01(WDGXKEY  =' W-4477-KEY-X ')'                        
021300            DELIMITED BY SIZE INTO SSA1                                   
021400     MOVE '  '             TO GODK-STATUSKODER                            
021500     CALL CBLTDLI USING GU XXLB-PCB DLI-IO-AREA SSA1                      
021600     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
021700     PERFORM IMS-STATUSKONTROLL                                           
021800     .                                                                    
021900     SKIP2                                                                
022000 IMS-GU-XXLB01-SDC SECTION.                                               
022100                                                                          
022200     STRING 'WLXXLB01(WDGXKEY  =' W-4477-KEY-X ')'                        
022300            DELIMITED BY SIZE INTO SSA1                                   
022400     MOVE '  GE'           TO GODK-STATUSKODER                            
022500     CALL CBLTDLI USING GU XXLB-PCB DLI-IO-AREA SSA1                      
022600     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
022700     PERFORM IMS-STATUSKONTROLL                                           
022800     .                                                                    
022900     SKIP2                                                                
023000 IMS-GNP-XXLB11 SECTION.                                                  
023100                                                                          
023200     STRING 'WLXXLB01(WDGXKEY  =' W-4477-KEY-X ')'                        
023300            DELIMITED BY SIZE INTO SSA1                                   
023400     STRING 'WLXXLB11   '                                                 
023500            DELIMITED BY SIZE INTO SSA2                                   
023600     MOVE '  GE'           TO GODK-STATUSKODER                            
023700     CALL CBLTDLI USING GNP XXLB-PCB DLI-IO-AREA SSA1 SSA2                
023800     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
023900     PERFORM IMS-STATUSKONTROLL                                           
024000     .                                                                    
024100     SKIP2                                                                
024200 IMS-STATUSKONTROLL SECTION.                                              
024300                                                                          
024400     SET STATUS-IX TO 1                                                   
024500     SEARCH GODK-STATUS                                                   
024600       AT END                                                             
024700         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
024800         DISPLAY FELTEXT                                                  
024900         CALL FELLOG                                                      
025000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025100         CONTINUE                                                         
025200     END-SEARCH                                                           
025300     .                                                                    
