001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4792200.                                                
001300 AUTHOR.         SUSANNE OLSSON.                                          
001400 DATE-WRITTEN.   00/06/29.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*    FUNKTION:                                                            
001900*        PROGRAMMET LÄSER IN FILER MED POSTER SOM SKALL RENSAS PÅ         
002000*        WDR4. RENSNINGSFILEN KOMMER FRÅN PGM W4792100.                   
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WDR4                                       
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- RENSNINGSPOSTER FÖR WDGX4102                               
003110     SELECT WDR44014                   ASSIGN TO W47922D1.                
003120*          --- RENSNINGSPOSTER FÖR WDGX4014                               
003130     SELECT WDR44102                   ASSIGN TO W47922D2.                
003140*          --- RENSNINGSPOSTER FÖR WDGX4319                               
003150     SELECT WDR44319                   ASSIGN TO W47922D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  WDR44014                                                             
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY WDR44014    -L.                                                
003720     SKIP3                                                                
003730 FD  WDR44102                                                             
003740     RECORDING       F                                                    
003750     BLOCK CONTAINS  0.                                                   
003760                                                                          
003770*01  -COPY WDR44102    -L.                                                
003780     SKIP3                                                                
003790 FD  WDR44319                                                             
003791     RECORDING       F                                                    
003792     BLOCK CONTAINS  0.                                                   
003793                                                                          
003794*01  -COPY WDR44319    -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W4792200'.            
004110 77  W-ANTAL-4014                PIC 9(4)    VALUE ZERO.                  
004120 77  W-ANTAL-4102                PIC 9(4)    VALUE ZERO.                  
004130 77  W-ANTAL-4319                PIC 9(4)    VALUE ZERO.                  
004131 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
004132 77  SPAR-IDDISTR                PIC S9(5)   VALUE +0   COMP-3.           
004140 77  JA                          PIC X       VALUE 'J'.                   
004150 77  NEJ                         PIC X       VALUE 'N'.                   
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800**SO 03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004801     03 CHKP-MAX                 PIC S9(3)   VALUE +50  COMP-3.           
005020                                                                          
005021 01  FELTEXT.                                                             
005022     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005023     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005024                                                                          
005030 77  FIRST-POST-SW               PIC X   VALUE 'J'.                       
005040     88  FIRST-POST                      VALUE 'J'.                       
005100     SKIP2                                                                
005602 77  WDR44014-EOF-SW             PIC X       VALUE 'N'.                   
005610     88  END-OF-WDR44014                     VALUE 'J'.                   
005620 77  WDR44102-EOF-SW             PIC X       VALUE 'N'.                   
005630     88  END-OF-WDR44102                     VALUE 'J'.                   
005640 77  WDR44319-EOF-SW             PIC X       VALUE 'N'.                   
005650     88  END-OF-WDR44319                     VALUE 'J'.                   
005900     EJECT                                                                
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
007402 01  IN1-AREA-START              PIC X(24)   VALUE                        
007403                                             'IN1-AREA-START'.            
007410*01  AREA -COPY WDR44014   -PRE IN1-                                      
007600     EJECT                                                                
007610 01  IN2-AREA-START              PIC X(24)   VALUE                        
007620                                             'IN2-AREA-START'.            
007630*01  AREA -COPY WDR44102   -PRE IN2-                                      
007640     EJECT                                                                
007650 01  IN3-AREA-START              PIC X(24)   VALUE                        
007660                                             'IN3-AREA-START'.            
007670*01  AREA -COPY WDR44319   -PRE IN3-                                      
007680     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008000                                                                          
008010*    03 -COPY WDGX01                                                      
008011       05 HTYP-4101 REDEFINES NYCKEL-VALFRI.                              
008012           07 W-IDDC               PIC X(2).                              
008013           07 W-IDDISTR            PIC S9(5)   COMP-3.                    
008014           07 FILLER               PIC X(21).                             
008015       05 HTYP-4319 REDEFINES NYCKEL-VALFRI.                              
008017           07 W-TIREGDAT           PIC S9(7)   COMP-3.                    
008018           07 FILLER               PIC X(22).                             
008019                                                                          
008062     03 W-IDGMTREF-X.                                                     
008063         05 W-IDGMTREF             PIC X(17)   VALUE SPACE.               
008064                                                                          
008080     03 W-WDGX4102-X.                                                     
008090         05 W-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.         
008091         05 W-IDRAPP               PIC X(10)   VALUE SPACE.               
008092         05 W-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.         
008093         05 W-IDARTNR              PIC S9(9)   VALUE ZERO COMP-3.         
008100     EJECT                                                                
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
010205 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
010220 01  DLI-IO-AREA.                                                         
010230     03  IO-AREA                 PIC X(1000).                             
010251*    03  -COPY WDGX4014 -RED IO-AREA.                                     
010252*    03  -COPY WDGX4101 -RED IO-AREA.                                     
010260*    03  -COPY WDGX4102 -RED IO-AREA.                                     
010270*    03  -COPY WDGX4319 -RED IO-AREA.                                     
010300                                                                          
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009  -PRE MSG-                                               
011101                                                                          
011102*01  -COPY W0008  -PRE WDR4-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB WDR4-PCB.                              
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB WDR4-PCB.                              
011600                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
011910                                                                          
012000     PERFORM B-RENSA-4014                                                 
013421     PERFORM C-RENSA-4102                                                 
013430     PERFORM D-RENSA-4319                                                 
013440                                                                          
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
014610     OPEN INPUT WDR44014                                                  
014620                WDR44102                                                  
014630                WDR44319                                                  
014900                                                                          
015200     MOVE +0    TO CHKP-ANT                                               
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     EJECT                                                                
015810 B-RENSA-4014 SECTION.                                                    
015811                                                                          
015812     MOVE JA TO FIRST-POST-SW                                             
015819                                                                          
015820     PERFORM S01-LAES-WDR44014                                            
015830     PERFORM UNTIL END-OF-WDR44014                                        
015831       IF FIRST-POST                                                      
015833         MOVE IN1-IDHTYP     TO IDHTYP                                    
015834         MOVE LOW-VALUE      TO NYCKEL-VALFRI                             
015835         PERFORM IMS-GU-WDR401                                            
015836         MOVE NEJ TO FIRST-POST-SW                                        
015837       END-IF                                                             
015838                                                                          
015840       IF CHKP-ANT > CHKP-MAX                                             
015851         PERFORM IMS-CHECKPOINT                                           
015852         MOVE ZERO TO CHKP-ANT                                            
015853         PERFORM IMS-GU-WDR401                                            
015860       END-IF                                                             
015870                                                                          
015895       MOVE IN1-IDGMTREF TO W-IDGMTREF                                    
015896       PERFORM IMS-GHNP-WDGX4014                                          
015897       IF SEGMENT-FINNS                                                   
015899         PERFORM IMS-DLET-WDR4X                                           
015900         ADD +1 TO CHKP-ANT                                               
015901                   W-ANTAL-4014                                           
015902       END-IF                                                             
015903                                                                          
015904       PERFORM S01-LAES-WDR44014                                          
015905     END-PERFORM                                                          
015906     .                                                                    
015907     EJECT                                                                
015908 C-RENSA-4102 SECTION.                                                    
015909                                                                          
015910     PERFORM S02-LAES-WDR44102                                            
015919     PERFORM UNTIL END-OF-WDR44102                                        
015920       IF IN2-IDDC    = SPAR-IDDC    AND                                  
015921          IN2-IDDISTR = SPAR-IDDISTR                                      
015922         CONTINUE                                                         
015923       ELSE                                                               
015924         MOVE IN2-IDHTYP     TO IDHTYP                                    
015925         MOVE LOW-VALUE      TO NYCKEL-VALFRI                             
015926         MOVE IN2-IDDC       TO W-IDDC                                    
015927                                SPAR-IDDC                                 
015928         MOVE IN2-IDDISTR    TO W-IDDISTR                                 
015929                                SPAR-IDDISTR                              
015930         PERFORM IMS-GU-WDR401                                            
015931       END-IF                                                             
015932                                                                          
015933       IF CHKP-ANT > CHKP-MAX                                             
015934         PERFORM IMS-CHECKPOINT                                           
015935         MOVE ZERO TO CHKP-ANT                                            
015936         PERFORM IMS-GU-WDR401                                            
015937       END-IF                                                             
015938                                                                          
015939       MOVE IN2-IDKUNDNR TO W-IDKUNDNR                                    
015940       MOVE IN2-IDRAPP   TO W-IDRAPP                                      
015941       MOVE IN2-IDKOLLI  TO W-IDKOLLI                                     
015942       MOVE IN2-IDARTNR  TO W-IDARTNR                                     
015943       PERFORM IMS-GHNP-WDGX4102                                          
015944       IF SEGMENT-FINNS                                                   
015945         PERFORM IMS-DLET-WDR4X                                           
015946         ADD +1 TO CHKP-ANT                                               
015947                   W-ANTAL-4102                                           
015948       END-IF                                                             
015949                                                                          
015950       PERFORM S02-LAES-WDR44102                                          
015951     END-PERFORM                                                          
015952     .                                                                    
015953     EJECT                                                                
015954 D-RENSA-4319 SECTION.                                                    
015955                                                                          
015956     PERFORM S03-LAES-WDR44319                                            
015957     PERFORM UNTIL END-OF-WDR44319                                        
015958                                                                          
015959       MOVE IN3-IDHTYP       TO IDHTYP                                    
015960       MOVE LOW-VALUE        TO NYCKEL-VALFRI                             
015961       MOVE IN3-TIREGDAT     TO W-TIREGDAT                                
015962       PERFORM IMS-GHU-WDR401                                             
015964       IF SEGMENT-FINNS                                                   
015966         PERFORM IMS-DLET-WDR4X                                           
015968         ADD +1 TO CHKP-ANT                                               
015969                   W-ANTAL-4319                                           
015970       END-IF                                                             
015971                                                                          
015972       IF CHKP-ANT > CHKP-MAX                                             
015973         PERFORM IMS-CHECKPOINT                                           
015974         MOVE ZERO TO CHKP-ANT                                            
015975       END-IF                                                             
015976                                                                          
015977       PERFORM S03-LAES-WDR44319                                          
015978     END-PERFORM                                                          
015979     .                                                                    
015980     EJECT                                                                
015981                                                                          
015990 Z-FINIT SECTION.                                                         
016401                                                                          
016410     CLOSE WDR44014                                                       
016420           WDR44102                                                       
016430           WDR44319                                                       
016431                                                                          
016440     DISPLAY 'ANTAL 4014-BORTTAG: ' W-ANTAL-4014                          
016450     DISPLAY 'ANTAL 4102-BORTTAG: ' W-ANTAL-4102                          
016460     DISPLAY 'ANTAL 4319-BORTTAG: ' W-ANTAL-4319                          
016601                                                                          
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901     EJECT                                                                
016902 S01-LAES-WDR44014 SECTION.                                               
016903     SKIP2                                                                
016904     READ WDR44014 INTO IN1-AREA                                          
016905     AT END                                                               
016907        SET END-OF-WDR44014 TO TRUE                                       
016908                                                                          
016909     NOT AT END                                                           
016910        MOVE 'WDR44014'  TO POSTSUM-FDNAMN                                
016911        MOVE 'W47922D1'  TO POSTSUM-DDNAMN2                               
016912        MOVE '4014'      TO POSTSUM-TRANSTYP                              
016913        CALL POSTSUM USING POSTSUM-PARM                                   
016916     END-READ                                                             
016920     .                                                                    
017200     EJECT                                                                
017210 S02-LAES-WDR44102 SECTION.                                               
017220     SKIP2                                                                
017230     READ WDR44102 INTO IN2-AREA                                          
017240     AT END                                                               
017250        SET END-OF-WDR44102 TO TRUE                                       
017260                                                                          
017270     NOT AT END                                                           
017280        MOVE 'WDR44102'  TO POSTSUM-FDNAMN                                
017290        MOVE 'W47922D2'  TO POSTSUM-DDNAMN2                               
017291        MOVE '4102'      TO POSTSUM-TRANSTYP                              
017292        CALL POSTSUM USING POSTSUM-PARM                                   
017293     END-READ                                                             
017294     .                                                                    
017295     EJECT                                                                
017296 S03-LAES-WDR44319 SECTION.                                               
017297     SKIP2                                                                
017298     READ WDR44319 INTO IN3-AREA                                          
017299     AT END                                                               
017300        SET END-OF-WDR44319 TO TRUE                                       
017301                                                                          
017302     NOT AT END                                                           
017303        MOVE 'WDR44319'  TO POSTSUM-FDNAMN                                
017304        MOVE 'W47922D3'  TO POSTSUM-DDNAMN2                               
017305        MOVE '4319'      TO POSTSUM-TRANSTYP                              
017306        CALL POSTSUM USING POSTSUM-PARM                                   
017307     END-READ                                                             
017308     .                                                                    
017309     EJECT                                                                
018500* --- IMS SEKTIONER ---                                                   
018600                                                                          
018722 IMS-GU-WDR401 SECTION.                                                   
018723                                                                          
018724     STRING 'WDR401  (WDGXKEY  =' WDGX01 ')'                              
018725          DELIMITED BY SIZE INTO SSA1                                     
018728     MOVE '    ' TO GODK-STATUSKODER                                      
018729     CALL CBLTDLI USING GU WDR4-PCB DLI-IO-AREA SSA1                      
018730     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
018731     PERFORM IMS-STATUSKONTROLL                                           
018732     .                                                                    
018733     SKIP3                                                                
018734 IMS-GHU-WDR401 SECTION.                                                  
018735                                                                          
018736     STRING 'WDR401  (WDGXKEY  =' WDGX01 ')'                              
018737          DELIMITED BY SIZE INTO SSA1                                     
018738     MOVE '  GE' TO GODK-STATUSKODER                                      
018739     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-AREA SSA1                     
018740     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
018741     PERFORM IMS-STATUSKONTROLL                                           
018742     .                                                                    
018743     EJECT                                                                
018744 IMS-GHNP-WDGX4102 SECTION.                                               
018745                                                                          
018748     STRING 'WDGX4102(KY4102   =' W-WDGX4102-X ')'                        
018749          DELIMITED BY SIZE INTO SSA1                                     
018750     MOVE '  GE' TO GODK-STATUSKODER                                      
018751     CALL CBLTDLI USING GHNP WDR4-PCB DLI-IO-AREA SSA1                    
018752     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
018753     PERFORM IMS-STATUSKONTROLL                                           
018754     .                                                                    
018755     SKIP3                                                                
018756 IMS-GHNP-WDGX4014 SECTION.                                               
018757                                                                          
018758     STRING 'WDGX4014(IDGMTREF =' W-IDGMTREF-X ')'                        
018759          DELIMITED BY SIZE INTO SSA1                                     
018760     MOVE '  GE' TO GODK-STATUSKODER                                      
018761     CALL CBLTDLI USING GHNP WDR4-PCB DLI-IO-AREA SSA1                    
018762     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
018763     PERFORM IMS-STATUSKONTROLL                                           
018764     .                                                                    
018765     SKIP3                                                                
018766 IMS-DLET-WDR4X SECTION.                                                  
018767                                                                          
018768     MOVE '  ' TO GODK-STATUSKODER                                        
018769     CALL CBLTDLI USING DLET WDR4-PCB DLI-IO-AREA                         
018770     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
018771     PERFORM IMS-STATUSKONTROLL                                           
018780     .                                                                    
018800     EJECT                                                                
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
