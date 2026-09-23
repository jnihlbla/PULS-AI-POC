001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4407200.                                                
001300*AUTHOR.         PER BERGH.                                               
001400*DATE-WRITTEN.   92/11/26.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LÄSER IDORDER FRÅN INFIL, KOMPLETTERAR MED DATA FRÅN WDE9        
002000*        OCH SKRIVER UTFIL MED KVOFFERT PER IDGMTREF                      
002100*                                                                         
002210*        PROGRAMMET LÄSER      WLPROD (WDE9)                              
002300*                                                                         
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- IDORDER SORTERAT URVAL FRÅN WDE8                           
003503     SELECT W4407S                     ASSIGN TO W44072D1.                
003504     SKIP2                                                                
003505*          --- URVAL KVOFFERT PER IDGMTREF/ARTNR FRÅN WDE9                
003510     SELECT W44072                     ASSIGN TO W44072D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W4407S                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004106*01  -COPY W440071      -L.                                               
004107     SKIP3                                                                
004108 FD  W44072                                                               
004109     RECORDING       F                                                    
004110     BLOCK CONTAINS  0.                                                   
004111     SKIP2                                                                
004120*01  POST -COPY W440072 -PRE  UT-  -L.                                    
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4407200'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004901                                                                          
004902 77  W4407S-EOF-SW               PIC X       VALUE 'N'.                   
004910     88  END-OF-W4407S                       VALUE 'J'.                   
005000     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     SKIP2                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START               PIC X(24)   VALUE                        
007403                                 'IN-AREA-START  '.                       
007404     SKIP2                                                                
007405                                                                          
007406*01  AREA -COPY W440071     -PRE IN-                                      
007407     EJECT                                                                
007408 01  UT-AREA-START               PIC X(24)   VALUE                        
007409                                 'UT-AREA-START  '.                       
007410     SKIP2                                                                
007411                                                                          
007420*01  AREA -COPY W440072     -PRE UT-                                      
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800     SKIP3                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-WDE901KY-MIN-X.                                                
008210         05  W-IDORDER-MIN       PIC S9(7)    VALUE ZERO  COMP-3.         
008230         05  FILLER              PIC  X(7)    VALUE LOW-VALUE.            
008300     SKIP2                                                                
008310     03  W-WDE901KY-MAX-X.                                                
008320         05  W-IDORDER-MAX       PIC S9(7)    VALUE ZERO  COMP-3.         
008330         05  FILLER              PIC  X(7)    VALUE HIGH-VALUE.           
008360     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008810     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA.                                                         
010501     SKIP3                                                                
010502*    03  -COPY WDE901                                                     
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011102*01  -COPY W0008  -PRE PROD-                                              
011110     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011301 PROCEDURE DIVISION  USING PROD-PCB.                                      
011310     ENTRY 'DLITCBL' USING PROD-PCB.                                      
011400                                                                          
011700     PERFORM A-INIT                                                       
011810     PERFORM S01-LAES-W4407S                                              
011900     PERFORM UNTIL END-OF-W4407S                                          
011910       MOVE IN-IDORDER TO                                                 
011920                          W-IDORDER-MIN                                   
011930                          W-IDORDER-MAX                                   
012000       PERFORM IMS-GU-PROD-WDE9                                           
012010       PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS OR                    
012020                     PRAD-IDORDER NOT = IN-IDORDER                        
012100                                                                          
012200         PERFORM B-FLYTTA-VAERDEN                                         
012300         PERFORM S11-SKRIV-W44072                                         
012310                                                                          
012400         PERFORM IMS-GN-PROD-WDE9                                         
012500       END-PERFORM                                                        
012610       PERFORM S01-LAES-W4407S                                            
012700     END-PERFORM                                                          
012900                                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013701                                                                          
013710     OPEN INPUT  W4407S                                                   
013801                                                                          
013810     OPEN OUTPUT W44072                                                   
013900     SKIP2                                                                
014110     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014300     .                                                                    
014400     EJECT                                                                
014410 B-FLYTTA-VAERDEN SECTION.                                                
014411     SKIP2                                                                
014412     MOVE IN-IDDISTR     TO  UT-IDDISTR                                   
014413     MOVE IN-IDKUNDNR    TO  UT-IDKUNDNR                                  
014414     MOVE IN-IDKUNDRF    TO  UT-IDKUNDRF                                  
014415     MOVE PRAD-IDARTNR   TO  UT-IDARTNR                                   
014417     MOVE PRAD-KVBEART-Q TO  UT-KVOFFERT                                  
014420     .                                                                    
014430     EJECT                                                                
014500 Z-FINIT SECTION.                                                         
014601     CLOSE W4407S                                                         
014610           W44072                                                         
014701     SKIP2                                                                
014702     MOVE 'S' TO POSTSUM-OPKOD                                            
014710     CALL POSTSUM USING POSTSUM-PARM                                      
014800     .                                                                    
014901     EJECT                                                                
014902 S01-LAES-W4407S  SECTION.                                                
014903     SKIP2                                                                
014904     READ W4407S INTO IN-AREA                                             
014905     AT END                                                               
014906*       MOVE HIGH-VALUE TO IN-ID                                          
014907        SET END-OF-W4407S TO TRUE                                         
014908                                                                          
014909     NOT AT END                                                           
014910        MOVE 'W4407S'   TO POSTSUM-FDNAMN                                 
014911        MOVE 'W44072D1' TO POSTSUM-DDNAMN2                                
014912        MOVE 'WDE8 '    TO POSTSUM-TRANSTYP                               
014913        CALL POSTSUM USING POSTSUM-PARM                                   
014914     END-READ                                                             
014920     .                                                                    
015001     EJECT                                                                
015002 S11-SKRIV-W44072 SECTION.                                                
015003     SKIP2                                                                
015004     WRITE UT-POST FROM UT-AREA                                           
015005                                                                          
015006     MOVE 'WDE9'     TO POSTSUM-TRANSTYP                                  
015007     MOVE 'W44072'   TO POSTSUM-FDNAMN                                    
015008     MOVE 'W44072D2' TO POSTSUM-DDNAMN2                                   
015009     CALL POSTSUM USING POSTSUM-PARM                                      
015010     .                                                                    
015200     EJECT                                                                
015900* --- IMS SEKTIONER ---                                                   
016000     SKIP3                                                                
016102 IMS-GU-PROD-WDE9 SECTION.                                                
016103     STRING 'WLPROD01(WDE901KY>=' W-WDE901KY-MIN-X                        
016104                    '&WDE901KY<=' W-WDE901KY-MAX-X ')'                    
016105          DELIMITED BY SIZE INTO SSA1                                     
016106     MOVE '  GE' TO GODK-STATUSKODER                                      
016107     CALL CBLTDLI USING GU PROD-PCB DLI-IO-AREA SSA1                      
016108     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
016109     PERFORM IMS-STATUSKONTROLL                                           
016110     .                                                                    
016200     SKIP2                                                                
016210 IMS-GN-PROD-WDE9 SECTION.                                                
016220     STRING 'WLPROD01(WDE901KY>=' W-WDE901KY-MIN-X                        
016230                    '&WDE901KY<=' W-WDE901KY-MAX-X ')'                    
016240          DELIMITED BY SIZE INTO SSA1                                     
016250     MOVE '  GE' TO GODK-STATUSKODER                                      
016260     CALL CBLTDLI USING GN PROD-PCB DLI-IO-AREA SSA1                      
016270     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
016280     PERFORM IMS-STATUSKONTROLL                                           
016290     .                                                                    
016291     SKIP2                                                                
016300 IMS-STATUSKONTROLL SECTION.                                              
016400     SKIP2                                                                
016500     SET STATUS-IX TO 1                                                   
016600     SEARCH GODK-STATUS                                                   
016700       AT END                                                             
017000         CALL FELLOG                                                      
017100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017200         CONTINUE                                                         
017300     END-SEARCH                                                           
017400     .                                                                    
