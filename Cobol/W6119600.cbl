001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W6119600.                                                
001200 AUTHOR.         KENT JEBSEN.                                             
001300 DATE-WRITTEN.   98/11/03.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        SB, LÄSER NER W6G123-SEGMENT PÅ FIL                              
001800*                                                                         
001910*        PROGRAMMET LÄSER      W6HANB (W6G1)                              
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- W6G123-SEGMENT                                             
003210     SELECT W61196                     ASSIGN TO W61196D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W61196                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  POST -COPY W6GX6036 -PRE  UT-  -L.                                   
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W6119600'.            
004210 01  FELTEXT.                                                             
004220     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004230     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004700     EJECT                                                                
004800*      --- VALID IDDC CODES                                               
004900*                                                                         
005000*01    -COPY WWDC99                                                       
005100       EJECT                                                              
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006100*    --- PARAMETRAR TILL ABEND                                            
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  W6G1-AREA-START             PIC X(24)   VALUE                        
007203                                 'W6G1-AREA-START  '.                     
007204     SKIP2                                                                
007205                                                                          
007210*01  AREA -COPY W6GX6036         -PRE UT-                                 
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008600     SKIP2                                                                
008700 01  GODK-STATUSKODER.                                                    
008800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     SKIP3                                                                
009000 01  SSA1                        PIC X(64).                               
009100 01  SSA2                        PIC X(64).                               
009200     EJECT                                                                
009300*    --- IMS FUNKTIONSKODER                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009811 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6HANB'.                      
009812     SKIP3                                                                
009813 01  DLI-IO-W6HANB.                                                       
009814     03  IO-AREA                 PIC X(30)  VALUE SPACE.                  
009815     SKIP3                                                                
009816     03  W6G101   REDEFINES IO-AREA.                                      
009817*        05  -COPY W6GX01                                                 
009818     SKIP3                                                                
009819     03  W6G123   REDEFINES IO-AREA.                                      
009820*        05  -COPY W6GX6036                                               
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010401                                                                          
010402*01  -COPY W0008  -PRE HANB-                                              
010410     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010601 PROCEDURE DIVISION  USING HANB-PCB.                                      
010602 MAIN SECTION.                                                            
010610     ENTRY 'DLITCBL' USING HANB-PCB.                                      
010700                                                                          
010900                                                                          
011000     PERFORM A-INIT                                                       
011100                                                                          
011201     PERFORM IMS-GET-HANB                                                 
011202     PERFORM UNTIL SEGMENT-SAKNAS                                         
011203       EVALUATE HANB-SEG-NAME-FB                                          
011204         WHEN 'W6G101'                                                    
011205             MOVE ROT-NYCKEL-VALFRI(1:2) TO WS-IDDC                       
011206         WHEN 'W6G123'                                                    
011207             IF CDC-SE                                                    
011208               MOVE 6036-W6GX6036 TO UT-6036-W6GX6036                     
011209               PERFORM S11-SKRIV-W61196                                   
011210             END-IF                                                       
011212       END-EVALUATE                                                       
011213       PERFORM IMS-GET-HANB                                               
011220     END-PERFORM                                                          
011300     PERFORM Z-FINIT                                                      
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012101                                                                          
012110     OPEN OUTPUT W61196                                                   
012200                                                                          
012410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     .                                                                    
012700     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012910     CLOSE W61196                                                         
013001     SKIP2                                                                
013002     MOVE 'S' TO POSTSUM-OPKOD                                            
013010     CALL POSTSUM USING POSTSUM-PARM                                      
013100     .                                                                    
013301     EJECT                                                                
013302 S11-SKRIV-W61196 SECTION.                                                
013303                                                                          
013304     WRITE UT-POST FROM UT-AREA                                           
013305                                                                          
013307     MOVE 'W61196' TO POSTSUM-FDNAMN                                      
013308     MOVE 'W61196D1' TO POSTSUM-DDNAMN2                                   
013309     CALL POSTSUM USING POSTSUM-PARM                                      
013310     .                                                                    
013500     EJECT                                                                
014200* --- IMS SEKTIONER ---                                                   
014401                                                                          
014402 IMS-GET-HANB   SECTION.                                                  
014403                                                                          
014404     CALL CBLTDLI USING GN HANB-PCB DLI-IO-W6HANB                         
014405     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
014406     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014407     PERFORM IMS-STATUSKONTROLL                                           
014410     .                                                                    
014500     EJECT                                                                
014600 IMS-STATUSKONTROLL SECTION.                                              
014700                                                                          
014800     SET STATUS-IX TO 1                                                   
014900     SEARCH GODK-STATUS                                                   
015000       AT END                                                             
015100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015200           DELIMITED BY SIZE INTO FELTEXT                                 
015300         DISPLAY FELTEXT                                                  
015400         CALL FELLOG                                                      
015500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015600         CONTINUE                                                         
015700     END-SEARCH                                                           
015800     .                                                                    
