000100 ID DIVISION.                                                             
000200 PROGRAM-ID.        W1222200.                                             
000300 AUTHOR.            ANDERS HENRIKSSON                                     
000400 DATE-WRITTEN.      2010-10-23                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*            RENSA ARTIKLAR                                               
000900*            FRÅN LEVERANSPLANER PÅ WDD9                                  
001400                                                                          
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*- - - - - - - - - - - - - - INFILER:                                     
002100     SELECT W12207         ASSIGN TO     W12222D1.                        
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500     SKIP3                                                                
002600 FD   W12207                                                              
002700      RECORDING F                                                         
002800      BLOCK CONTAINS 0.                                                   
002900*01   POST -COPY W12207   -PRE W12207-    -L.                             
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W1222200'.            
003410 01  CHKP-VAR.                                                            
003420 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
003430 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
003440 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
003450 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
003460 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
003470 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
003500 01  GENERELLA-KONSTANTER.                                                
003600   03  JA                        PIC X       VALUE 'J'.                   
003700   03  NEJ                       PIC X       VALUE 'N'.                   
003800   03  W12207-EOF                PIC X       VALUE 'N'.                   
003800   03  W-DLET-WDD901             PIC 9(7)    VALUE ZERO.                  
003900     SKIP2                                                                
003910 01  FELTEXT.                                                             
003920     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003930     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003940                                                                          
003950*01   -COPY WWDCKONS                                                      
003960                                                                          
004000 01  DYNAMISKA-SUBPROGRAM.                                                
004100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
004200   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
004300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004500     EJECT                                                                
004600*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
004700*01  -COPY W0005     -PRE  POSTSUM-.                                      
004800     EJECT                                                                
004900*- - - - - - - - - - - - - -  ARBETSAREAR FÖR INFIL                       
005000 01  FILLER                      PIC X(24)    VALUE 'INFIL'.              
005100                                                                          
005200*01  AREA    -COPY W12207   -PRE W-IN-.                                   
005300     EJECT                                                                
005400*- - - - - - - - -PARAMETER-AREOR TILL IMS-SUBPGM-SEKTIONER.              
005500 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
005600     SKIP3                                                                
005700*- - - - - - - - -STATUSKOD FRÅN IMS                                      
005800 01  STATUS-WS                   PIC X(2).                                
005900   88  SEGMENT-FINNS         VALUE '  '.                                  
006100   88  SEGMENT-FINNS-REDAN   VALUE 'II'.                                  
006101   88  SEGMENT-SAKNAS        VALUE 'GE'.                                  
006110   88  SEGMENT-SLUT          VALUE 'GB'.                                  
006120   88  IMS-EJ-OK             VALUE 'XD'.                                  
006200   SKIP3                                                                  
006300 01  GODK-STATUSKODER.                                                    
006400    03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
006500 01  SSA1                        PIC X(32).                               
006600 01  SSA2                        PIC X(32).                               
006700 01  SSA3                        PIC X(32).                               
006800*- - - - - - - - - - - - - - NYCKLAR TILL DLI.                            
006900 01  NYCKLAR-TILL-DLI.                                                    
007000   03   W-WDD901KY-X.                                                     
007100     05 W-IDARTNR                PIC S9(9)  COMP-3.                       
007110     05 W-IDDC                   PIC X(2).                                
007200     EJECT                                                                
007300*01   -COPY W0003                                                         
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)    VALUE                       
007600                                              'DLI-IO-AREA'.              
007700 01  DLI-IO-AREA.                                                         
007900   03  WDD901  -COPY WDD901                                               
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008210*01  -COPY W0009  -PRE MSG-                                               
008220     EJECT                                                                
008300*01  -COPY W0008       -PRE WDD9-.                                        
008400 05  FILLER             PIC X.                                            
008500     EJECT                                                                
008600 PROCEDURE DIVISION USING MSG-PCB WDD9-PCB.                               
008700 MAIN SECTION.                                                            
008800     ENTRY 'DLITCBL' USING MSG-PCB WDD9-PCB.                              
008900                                                                          
009000     PERFORM A-INIT                                                       
009100     PERFORM S11-LAES-INFIL                                               
009200     PERFORM UNTIL W12207-EOF = JA                                        
009300       IF W-IN-UTFIL-TYP = 'B'                                            
009310       OR W-IN-UTFIL-TYP = 'S'                                            
009400         MOVE W-IN-IDARTNR  TO W-IDARTNR                                  
009410         MOVE WC-CDC-SE     TO W-IDDC                                     
009500         PERFORM IMS-GHU-WDD901                                           
009600         IF SEGMENT-FINNS                                                 
009700           PERFORM IMS-DELETE-WDD9                                        
                 ADD 1 TO W-DLET-WDD901                                         
009710           ADD +1 TO CHKP-ANT                                             
009720         END-IF                                                           
009730         IF CHKP-ANT > CHKP-MAX                                           
009740           PERFORM X-TAG-CHECKPOINT                                       
009800         END-IF                                                           
009900       END-IF                                                             
010000       PERFORM S11-LAES-INFIL                                             
010100     END-PERFORM                                                          
010200                                                                          
010300     PERFORM Z-FINIT                                                      
010400     MOVE ZERO TO RETURN-CODE                                             
010500     GOBACK                                                               
010600     .                                                                    
010700     EJECT                                                                
010710                                                                          
010800 A-INIT SECTION.                                                          
010900     PERFORM IMS-RESTART                                                  
011000     OPEN  INPUT W12207                                                   
011100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011200     .                                                                    
011300     EJECT                                                                
011310                                                                          
011400 S11-LAES-INFIL SECTION.                                                  
011600     READ W12207 INTO W-IN-AREA                                           
011700         AT END MOVE JA    TO W12207-EOF                                  
011800     END-READ                                                             
011900     IF W12207-EOF = NEJ                                                  
012000       MOVE 'W12222D1'   TO POSTSUM-DDNAMN2                               
012100       MOVE 'W12207'     TO POSTSUM-FDNAMN                                
012200       CALL POSTSUM USING POSTSUM-PARM                                    
012300     END-IF                                                               
012400     .                                                                    
012500     EJECT                                                                
012501                                                                          
012510 X-TAG-CHECKPOINT SECTION.                                                
012530     PERFORM IMS-CHECKPOINT                                               
012540     MOVE ZERO TO CHKP-ANT                                                
012550     .                                                                    
012560     EJECT                                                                
012570                                                                          
012600 Z-FINIT SECTION.                                                         
           DISPLAY 'ANTAL BORTTAGNA WDD901: ' W-DLET-WDD901                     
012800     CLOSE W12207                                                         
012900     MOVE 'S' TO POSTSUM-OPKOD                                            
013000     CALL POSTSUM USING POSTSUM-PARM                                      
013100     .                                                                    
013200     EJECT                                                                
013300*- - - - - - - - - - - - - - - - - - - -IMS-SECTIONER.                    
013400 IMS-GHU-WDD901 SECTION.                                                  
013500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
013600            DELIMITED BY SIZE INTO SSA1                                   
013700     MOVE '  GE'              TO GODK-STATUSKODER                         
013800     CALL CBLTDLI USING GHU   WDD9-PCB DLI-IO-AREA SSA1                   
013900     MOVE WDD9-STATUS-CODE  TO STATUS-WS                                  
014000     PERFORM IMS-STATUSKONTROLL                                           
014100     .                                                                    
014200     SKIP3                                                                
014210                                                                          
014300 IMS-DELETE-WDD9 SECTION.                                                 
014400     MOVE '  '              TO GODK-STATUSKODER                           
014500     CALL CBLTDLI USING DLET  WDD9-PCB DLI-IO-AREA                        
014600     MOVE WDD9-STATUS-CODE  TO STATUS-WS                                  
014700     PERFORM IMS-STATUSKONTROLL                                           
014800     .                                                                    
014900     SKIP3                                                                
014910                                                                          
014920 IMS-RESTART SECTION.                                                     
014930     SKIP2                                                                
014940     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
014950     MOVE '  ' TO GODK-STATUSKODER                                        
014960     CALL CBLTDLI USING XRST MSG-PCB                                      
014970                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
014980                        CHKP-AREA-LENGTH CHKP-AREA                        
014990     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
014991     PERFORM IMS-STATUSKONTROLL                                           
014992     .                                                                    
014993     SKIP3                                                                
014994                                                                          
014995 IMS-CHECKPOINT SECTION.                                                  
014996     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
014997     MOVE '  XD' TO GODK-STATUSKODER                                      
014998     CALL CBLTDLI USING CHKP MSG-PCB                                      
014999                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
015000                        CHKP-AREA-LENGTH CHKP-AREA                        
015001     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
015002     PERFORM IMS-STATUSKONTROLL                                           
015003     IF IMS-EJ-OK                                                         
015004       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
015005       DISPLAY FELTEXT                                                    
015006       CALL FELLOG                                                        
015007     END-IF                                                               
015008     .                                                                    
015009     SKIP3                                                                
015010                                                                          
015020 IMS-STATUSKONTROLL SECTION.                                              
015100     SET STATUS-IX TO 1                                                   
015200     SEARCH GODK-STATUS                                                   
015300       AT END                                                             
015400         CALL FELLOG                                                      
015500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015600         CONTINUE                                                         
015700     END-SEARCH                                                           
015800     .                                                                    
