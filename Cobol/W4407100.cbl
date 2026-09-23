001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4407100.                                                
001300*AUTHOR.         PER BERGH.                                               
001400*DATE-WRITTEN.   92/11/25.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        SB SOM LÄSER WDE8 OCH SKRIVER FIL MED UTVALDA                    
002000*        PROFORMA-HUVUDEN                                                 
002100*                                                                         
002210*        PROGRAMMET LÄSER      WLPROC (WDE8)                              
002300*                                                                         
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- URVAL AV IDORDER UR WDE8                                   
003510     SELECT W44071                     ASSIGN TO W44071D1.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W44071                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004110*01  POST -COPY W440071 -PRE  UT-  -L.                                    
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4407100'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
005000     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  UT-AREA-START               PIC X(24)   VALUE                        
007403                                 'UT-AREA-START  '.                       
007404     SKIP2                                                                
007405                                                                          
007410*01  AREA -COPY W440071     -PRE UT-                                      
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010100     SKIP3                                                                
010200 01  DLI-IO-AREA.                                                         
010401     SKIP3                                                                
010402*    03  -COPY WDE801                                                     
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011001     EJECT                                                                
011002*01  -COPY W0008  -PRE PROC-                                              
011010     05  FILLER                  PIC X.                                   
011100     EJECT                                                                
011201 PROCEDURE DIVISION  USING PROC-PCB.                                      
011210     ENTRY 'DLITCBL' USING PROC-PCB.                                      
011300 STYR SECTION.                                                            
011500                                                                          
011600     PERFORM A-INIT                                                       
011700     PERFORM IMS-GET-WDE8                                                 
011800     PERFORM UNTIL SEGMENT-SLUT                                           
011900       IF PROC-SEG-NAME-FB = 'WDE801  '                                   
012000         IF (PHUV-KDPROTYP = 'L' OR 'O')  AND                             
012100            (PHUV-TIORDDAT = ZERO)                                        
012110            PERFORM B-SKRIV-W44071                                        
012300         END-IF                                                           
012310       END-IF                                                             
012320                                                                          
012400       PERFORM IMS-GET-WDE8                                               
012600     END-PERFORM                                                          
012800                                                                          
012900     PERFORM Z-FINIT                                                      
013000                                                                          
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 A-INIT SECTION.                                                          
013701                                                                          
013710     OPEN OUTPUT W44071                                                   
013800     SKIP2                                                                
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014310 B-SKRIV-W44071 SECTION.                                                  
014320     SKIP2                                                                
014330     MOVE PHUV-IDORDER  TO   UT-IDORDER                                   
014331     MOVE PHUV-IDDISTR  TO   UT-IDDISTR                                   
014332     MOVE PHUV-IDKUNDNR TO   UT-IDKUNDNR                                  
014333     MOVE PHUV-IDKUNDRF TO   UT-IDKUNDRF                                  
014340     WRITE UT-POST      FROM UT-AREA                                      
014350                                                                          
014360     MOVE 'WDE8'     TO POSTSUM-TRANSTYP                                  
014370     MOVE 'W44071'   TO POSTSUM-FDNAMN                                    
014380     MOVE 'W44071D1' TO POSTSUM-DDNAMN2                                   
014390     CALL POSTSUM USING POSTSUM-PARM                                      
014391     .                                                                    
014392     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014510     CLOSE W44071                                                         
014601     SKIP2                                                                
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
015700     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016002 IMS-GET-WDE8   SECTION.                                                  
016003     SKIP2                                                                
016004     CALL CBLTDLI USING GN PROC-PCB DLI-IO-AREA                           
016005     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
016006     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016007     PERFORM IMS-STATUSKONTROLL                                           
016010     .                                                                    
016100     EJECT                                                                
016200 IMS-STATUSKONTROLL SECTION.                                              
016300     SKIP2                                                                
016400     SET STATUS-IX TO 1                                                   
016500     SEARCH GODK-STATUS                                                   
016600       AT END                                                             
016900         CALL FELLOG                                                      
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    
