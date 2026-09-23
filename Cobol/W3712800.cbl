001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W3712800.                                                
001300 AUTHOR.         GAVIN SMITH.                                             
001400 DATE-WRITTEN.   99/12/17.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900*        PROGRAM WHICH UPDATES WDA7 , THE EXCHANGE INVOICE/CRED           
002000*        DATABASE WITH DAILY TRANSACTIONS FROM 4XX SYSTEMS                
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WDA7, INGÅR I RUTIN W371D5                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- DAILY INVOICE FILE FROM 475 SYSTEM                         
003103*          --- DAILY CREDIT  FILE FROM 418 SYSTEM                         
003110     SELECT INFILE                     ASSIGN TO W37128D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  INFILE                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY W371FAK      -L.                                               
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W3712800'.            
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +50 COMP-3.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100     SKIP2                                                                
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005601 01  W-POST-IN                   PIC S9(6)   VALUE ZERO.                  
005602 77  INFILE-EOF-SW               PIC X       VALUE 'N'.                   
005610     88  END-OF-INFILE                       VALUE 'J'.                   
005900     EJECT                                                                
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     EJECT                                                                
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
007402 01  FAK-AREA-START              PIC X(24)   VALUE                        
007403                                             'FAK-AREA-START'.            
007404     SKIP2                                                                
007405                                                                          
007410*01  AREA -COPY W371FAK     -PRE FAK-                                     
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-WDA7KEY-X.                                                     
008010         05  W-WDA7KEY           PIC X(18)    VALUE SPACE.                
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
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
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA701'.                      
010202 01  DLI-IO-WDA701.                                                       
010210*    03  -COPY WDA701                                                     
010300                                                                          
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009   -PRE MSG-                                              
011101                                                                          
011102*01  -COPY W0008  -PRE WDA7-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB WDA7-PCB.                              
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB WDA7-PCB.                              
011600                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
012010     PERFORM S01-LAES-INFILE                                              
012100     PERFORM UNTIL END-OF-INFILE                                          
012200       IF CHKP-ANT > CHKP-MAX                                             
012300         PERFORM X-TAG-CHECKPOINT                                         
012400       END-IF                                                             
012500       PERFORM B-UPDAT-WDA7                                               
013000       ADD 1 TO CHKP-ANT                                                  
013110       PERFORM S01-LAES-INFILE                                            
013200     END-PERFORM                                                          
013300                                                                          
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
014400     PERFORM IMS-RESTART                                                  
014601                                                                          
014610     OPEN INPUT INFILE                                                    
014900                                                                          
015200                                                                          
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     EJECT                                                                
015810 B-UPDAT-WDA7 SECTION.                                                    
015811                                                                          
015812     MOVE   FAK-IDPTYP         TO ROT-IDPTYP                              
015813     MOVE   FAK-IDDISTR        TO ROT-IDDISTR                             
015814     MOVE   FAK-IDARTNR        TO ROT-IDARTNR-BYT                         
015815     MOVE   FAK-IDKUNDNR       TO ROT-IDKUNDNR                            
015816     MOVE   1                  TO ROT-IDBYTRAD                            
015817     MOVE   FUNCTION CURRENT-DATE (1:8)   TO ROT-DAFAKT                   
015818     MOVE   ZERO                     TO ROT-DAREGDAT                      
015819     MOVE   FAK-IDDC           TO ROT-IDDC                                
015820     MOVE   FAK-IDORDNR        TO ROT-IDORDER                             
015821     MOVE   'W37128'           TO ROT-IDUSER                              
015822     MOVE   ZERO               TO ROT-KDEXCHA                             
015823     MOVE   FAK-KVLEVART       TO ROT-KVANTAL                             
015824     MOVE   ZERO               TO ROT-KVPOINT                             
015825     MOVE   ZERO               TO ROT-TIKLOCK                             
015826     MOVE   SPACE              TO ROT-TENOTE                              
015827     MOVE   SPACE              TO ROT-FILLER                              
015828                                                                          
015829     PERFORM IMS-ISRT-WDA701                                              
015830     PERFORM UNTIL SEGMENT-FINNS                                          
015831      IF SEGMENT-FINNS-REDAN                                              
015832        ADD 1 TO ROT-IDBYTRAD                                             
015833        PERFORM IMS-ISRT-WDA701                                           
015834      END-IF                                                              
015835     END-PERFORM                                                          
015836     .                                                                    
015840     EJECT                                                                
015900 Z-FINIT SECTION.                                                         
016000                                                                          
016401                                                                          
016410     CLOSE INFILE                                                         
016601     SKIP2                                                                
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901     EJECT                                                                
016902 S01-LAES-INFILE  SECTION.                                                
016903     SKIP2                                                                
016904     READ INFILE INTO FAK-AREA                                            
016905     AT END                                                               
016907        SET END-OF-INFILE TO TRUE                                         
016908                                                                          
016909     NOT AT END                                                           
016910        MOVE 'INFILE' TO POSTSUM-FDNAMN                                   
016911        MOVE 'W37128D1' TO POSTSUM-DDNAMN2                                
016912        MOVE FAK-IDPTYP TO POSTSUM-TRANSTYP                               
016913        CALL POSTSUM USING POSTSUM-PARM                                   
016914                                                                          
016915        ADD 1 TO W-POST-IN                                                
016916     END-READ                                                             
016920     .                                                                    
017200     EJECT                                                                
017300 X-TAG-CHECKPOINT   SECTION.                                              
017400                                                                          
017500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
017600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
018000     PERFORM IMS-CHECKPOINT                                               
018100     MOVE ZERO TO CHKP-ANT                                                
018200* --- LÄS OM DATABAS OM DET BEHÖVS                                        
018300     .                                                                    
018400     EJECT                                                                
018500* --- IMS SEKTIONER ---                                                   
018600                                                                          
018701     EJECT                                                                
018712 IMS-ISRT-WDA701 SECTION.                                                 
018713                                                                          
018714     MOVE 'WDA701 ' TO SSA1                                               
018715     MOVE '  II' TO GODK-STATUSKODER                                      
018716     CALL CBLTDLI USING ISRT WDA7-PCB DLI-IO-WDA701 SSA1                  
018717     MOVE WDA7-STATUS-CODE TO STATUS-WS                                   
018718     PERFORM IMS-STATUSKONTROLL                                           
018719     .                                                                    
018720     SKIP3                                                                
018900 IMS-RESTART SECTION.                                                     
019000     SKIP2                                                                
019100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019200     MOVE '  ' TO GODK-STATUSKODER                                        
019300     CALL CBLTDLI USING XRST MSG-PCB                                      
019400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019500                        CHKP-AREA-LENGTH CHKP-AREA                        
019600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019700     PERFORM IMS-STATUSKONTROLL                                           
019800     .                                                                    
019900     SKIP3                                                                
020000 IMS-CHECKPOINT SECTION.                                                  
020100     SKIP2                                                                
020200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020300     MOVE '  XD' TO GODK-STATUSKODER                                      
020400     CALL CBLTDLI USING CHKP MSG-PCB                                      
020500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020600                        CHKP-AREA-LENGTH CHKP-AREA                        
020700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800     PERFORM IMS-STATUSKONTROLL                                           
020900                                                                          
021000     IF IMS-EJ-OK                                                         
021100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021200       DISPLAY FELTEXT                                                    
021300       CALL FELLOG                                                        
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
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
