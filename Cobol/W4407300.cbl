001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4407300.                                                
001300*AUTHOR.         PER BERGH.                                               
001400*DATE-WRITTEN.   92/11/27.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LÄSER INFIL MED IDORDER FRÅN WDE8, LÄSER OCH GÖR URVAL           
002000*        PÅ WDQ1 OCH SKRIVER UTFIL                                        
002100*                                                                         
002210*        PROGRAMMET LÄSER      WLORQM (WDQ1)                              
002300*                                                                         
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- IDORDER SORTERAD FRÅN WDE8                                 
003503     SELECT W4407S                     ASSIGN TO W44073D1.                
003504     SKIP2                                                                
003505*          --- URVAL KVOFFERT PER IDGMTREF/ARTNR/CLAGER FRÅN WDQ1         
003510     SELECT W44073                     ASSIGN TO W44073D2.                
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
004108 FD  W44073                                                               
004109     RECORDING       F                                                    
004110     BLOCK CONTAINS  0.                                                   
004111     SKIP2                                                                
004120*01  POST -COPY W440072 -PRE  UT-  -L.                                    
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4407300'.            
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
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-WDQ101KY-MIN-X.                                                
008210         05  W-IDORDER-MIN       PIC S9(7)    VALUE ZEROS  COMP-3.        
008220         05  FILLER              PIC X(13)    VALUE LOW-VALUE.            
008300     SKIP2                                                                
008310     03  W-WDQ101KY-MAX-X.                                                
008320         05  W-IDORDER-MAX       PIC S9(7)    VALUE ZEROS  COMP-3.        
008330         05  FILLER              PIC X(13)    VALUE HIGH-VALUE.           
008380     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008810     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(80).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA.                                                         
010501     SKIP3                                                                
010502*    03   -COPY WDQ101                                                    
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011101     EJECT                                                                
011102*01  -COPY W0008  -PRE ORQM-                                              
011110     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011301 PROCEDURE DIVISION  USING ORQM-PCB.                                      
011310     ENTRY 'DLITCBL' USING ORQM-PCB.                                      
011311                                                                          
011312     PERFORM A-INIT                                                       
011313     PERFORM S01-LAES-W4407S                                              
011314     PERFORM UNTIL END-OF-W4407S                                          
011315       MOVE IN-IDORDER TO W-IDORDER-MIN                                   
011317                          W-IDORDER-MAX                                   
011318       PERFORM IMS-GU-ORQM-WDQ1                                           
011319       PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS OR                    
011320                     OBKR-IDORDER NOT = IN-IDORDER                        
011322         IF OBKR-FLOBOK   = NEJ     AND                                   
011323            OBKR-IDSYSTEM = 'PROF'  AND                                   
011324            OBKR-KVPREAVB > ZERO                                          
011325                                                                          
011326            PERFORM B-FLYTTA-VAERDEN                                      
011327            PERFORM S11-SKRIV-W44073                                      
011328         END-IF                                                           
011329         PERFORM IMS-GN-ORQM-WDQ1                                         
011330       END-PERFORM                                                        
011331       PERFORM S01-LAES-W4407S                                            
011332     END-PERFORM                                                          
011333                                                                          
011334     PERFORM Z-FINIT                                                      
011335                                                                          
011336     MOVE ZERO TO RETURN-CODE                                             
011337     GOBACK                                                               
011338     .                                                                    
011339     EJECT                                                                
011340 A-INIT SECTION.                                                          
011341                                                                          
011342     OPEN INPUT  W4407S                                                   
011343                                                                          
011344     OPEN OUTPUT W44073                                                   
011345     SKIP2                                                                
011347     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011350     .                                                                    
011351     EJECT                                                                
011352 B-FLYTTA-VAERDEN SECTION.                                                
011353     SKIP2                                                                
011354     MOVE IN-IDDISTR     TO  UT-IDDISTR                                   
011355     MOVE IN-IDKUNDNR    TO  UT-IDKUNDNR                                  
011356     MOVE IN-IDKUNDRF    TO  UT-IDKUNDRF                                  
011359     MOVE OBKR-KVPREAVB  TO  UT-KVOFFERT                                  
011360     IF OBKR-IDARTNR-TILLK > 0                                            
011361       MOVE OBKR-IDARTNR-TILLK  TO  UT-IDARTNR                            
011362     ELSE                                                                 
011363       MOVE OBKR-IDARTNR        TO  UT-IDARTNR                            
011364     END-IF                                                               
011366     .                                                                    
011370     EJECT                                                                
014500 Z-FINIT SECTION.                                                         
014601     CLOSE W4407S                                                         
014610           W44073                                                         
014701     SKIP2                                                                
014702     MOVE 'S' TO POSTSUM-OPKOD                                            
014710     CALL POSTSUM USING POSTSUM-PARM                                      
014800     .                                                                    
014901     EJECT                                                                
014902 S01-LAES-W4407S  SECTION.                                                
014903     SKIP2                                                                
014904     READ W4407S INTO IN-AREA                                             
014905     AT END                                                               
014907        SET END-OF-W4407S TO TRUE                                         
014908                                                                          
014909     NOT AT END                                                           
014910        MOVE 'W4407S'   TO POSTSUM-FDNAMN                                 
014911        MOVE 'W44073D1' TO POSTSUM-DDNAMN2                                
014912        MOVE 'WDE8 '    TO POSTSUM-TRANSTYP                               
014913        CALL POSTSUM USING POSTSUM-PARM                                   
014914     END-READ                                                             
014920     .                                                                    
015001     EJECT                                                                
015002 S11-SKRIV-W44073 SECTION.                                                
015003     SKIP2                                                                
015004     WRITE UT-POST FROM UT-AREA                                           
015005                                                                          
015006     MOVE 'WDE9'     TO POSTSUM-TRANSTYP                                  
015007     MOVE 'W44073'   TO POSTSUM-FDNAMN                                    
015008     MOVE 'W44073D2' TO POSTSUM-DDNAMN2                                   
015009     CALL POSTSUM USING POSTSUM-PARM                                      
015010     .                                                                    
015200     EJECT                                                                
015900* --- IMS SEKTIONER ---                                                   
016000     SKIP3                                                                
016102 IMS-GU-ORQM-WDQ1 SECTION.                                                
016103     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
016104                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
016105          DELIMITED BY SIZE INTO SSA1                                     
016106     MOVE '  GE' TO GODK-STATUSKODER                                      
016107     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA SSA1                      
016108     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
016109     PERFORM IMS-STATUSKONTROLL                                           
016110     .                                                                    
016200     SKIP2                                                                
016210 IMS-GN-ORQM-WDQ1 SECTION.                                                
016220     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
016230                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
016240          DELIMITED BY SIZE INTO SSA1                                     
016250     MOVE '  GE' TO GODK-STATUSKODER                                      
016260     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA SSA1                      
016270     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
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
