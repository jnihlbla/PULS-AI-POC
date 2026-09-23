001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4322000.                                                
001300 AUTHOR.         STEFAN KIHLBERG.                                         
001400 DATE-WRITTEN.   00/10/11.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900*        UPPDATERAR KUNDREGISTRET WDB7 MED UPPGIFTER FRÅN VIPS            
002000*        KUNDREFISTER                                                     
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WDB7                                       
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- KUNDREGISTER FRÅN VIPS                                     
003110     SELECT W43220                     ASSIGN TO W43220D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  W43220                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY WDB701      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W4322000'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100     SKIP2                                                                
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005601                                                                          
005602 77  W43220-EOF-SW               PIC X       VALUE 'N'.                   
005610     88  END-OF-W43220                       VALUE 'J'.                   
005900     EJECT                                                                
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     EJECT                                                                
006530                                                                          
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START             PIC X(24)   VALUE                          
007403                                             'IN-AREA-START'.             
007404     SKIP2                                                                
007405                                                                          
007410*01  -COPY WDB701  -PRE IN-                                               
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-IDGMT-X.                                                       
008010         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
008020         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008710     88  DUBBLA-POSTER-I-LOADFIL             VALUE 'LB'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB701'.                      
010202 01  DLI-IO-WDB701.                                                       
010210*    03  -COPY WDB701                                                     
010300                                                                          
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011102*01  -COPY W0008  -PRE WDB7-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING  WDB7-PCB.                                     
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING  WDB7-PCB.                                     
011600                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
012010     PERFORM S01-LAES-W43220                                              
012100     PERFORM UNTIL END-OF-W43220                                          
012431       MOVE IN-GMTD-IDDISTR      TO W-IDDISTR                             
012432       MOVE IN-GMTD-IDKUNDNR     TO W-IDKUNDNR                            
012433       MOVE IN-GMTD-WDB701 TO GMTD-WDB701                                 
012434       PERFORM IMS-ISRT-WDB701                                            
012435       IF  DUBBLA-POSTER-I-LOADFIL                                        
012436           DISPLAY 'OBS OBS OBS'                                          
012437           DISPLAY "DUBBLA POSTER I LOADFIL"                              
012438           DISPLAY "DISTRIKT = " IN-GMTD-IDDISTR                          
012439           DISPLAY "KUNDNR   = " IN-GMTD-IDKUNDNR                         
012440       END-IF                                                             
012700       PERFORM S01-LAES-W43220                                            
012800     END-PERFORM                                                          
012900                                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200     SKIP2                                                                
014300                                                                          
014601                                                                          
014610     OPEN INPUT W43220                                                    
014900                                                                          
015200                                                                          
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     SKIP2                                                                
015801                                                                          
015900 Z-FINIT SECTION.                                                         
016000                                                                          
016401                                                                          
016410     CLOSE W43220                                                         
016601     SKIP2                                                                
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901     EJECT                                                                
016902 S01-LAES-W43220  SECTION.                                                
016903     SKIP2                                                                
016904     READ W43220 INTO IN-GMTD-WDB701                                      
016905     AT END                                                               
016907        SET END-OF-W43220 TO TRUE                                         
016908                                                                          
016909     NOT AT END                                                           
016910        MOVE 'W43220' TO POSTSUM-FDNAMN                                   
016911        MOVE 'W43220D1' TO POSTSUM-DDNAMN2                                
016914                                                                          
016916     END-READ                                                             
016920     .                                                                    
017200     EJECT                                                                
018500* --- IMS SEKTIONER ---                                                   
018600                                                                          
018721 IMS-ISRT-WDB701 SECTION.                                                 
018722                                                                          
018725     MOVE 'WDB701   ' TO SSA1                                             
018726     MOVE '  LB' TO GODK-STATUSKODER                                      
018727     CALL CBLTDLI USING ISRT WDB7-PCB DLI-IO-WDB701 SSA1                  
018728     MOVE WDB7-STATUS-CODE TO STATUS-WS                                   
018729     PERFORM IMS-STATUSKONTROLL                                           
018730     .                                                                    
018731     SKIP3                                                                
021700 IMS-STATUSKONTROLL SECTION.                                              
021800     SKIP2                                                                
021900     SET STATUS-IX TO 1                                                   
022000     SEARCH GODK-STATUS                                                   
022100       AT END                                                             
022200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022300           DELIMITED BY SIZE INTO FELTEXT                                 
022400         DISPLAY FELTEXT                                                  
022500         CALL FELLOG                                                      
022600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
