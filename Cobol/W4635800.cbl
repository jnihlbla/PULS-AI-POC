000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4635800.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   99/04/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER IN EN FELFIL OCH EDI FILEN. TAR BORT ALLA FELAKTIGA        
001000*        PRODNUMMER.                                                      
001100*                                                                         
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
002401     SKIP2                                                                
002402*          --- INFIL-1 EDI-FILEN                                          
002403     SELECT W46357                     ASSIGN TO W46358D1.                
002404     SKIP2                                                                
002405*          --- INFIL-2 FELPOSTER                                          
002406     SELECT W46358                     ASSIGN TO W46358D2.                
002407     SKIP2                                                                
002408*          --- UTFIL-1 RATTPOSTER                                         
002409     SELECT W46359                     ASSIGN TO W46358D3.                
002410     SKIP2                                                                
002411*          --- UTFIL-2 FELPOSTER                                          
002420     SELECT W46354                     ASSIGN TO W46358D4.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W46357                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W46357      -L.                                                
003007     SKIP3                                                                
003008 FD  W46358                                                               
003009     RECORDING       V                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003012*01  -COPY W46358      -L.                                                
003013     SKIP3                                                                
003014 FD  W46359                                                               
003015     RECORDING       F                                                    
003016     BLOCK CONTAINS  0.                                                   
003017                                                                          
003018*01  POST -COPY W46357 -PRE  EDI-  -L.                                    
003019     SKIP3                                                                
003020 FD  W46354                                                               
003021     RECORDING       V                                                    
003022     BLOCK CONTAINS  0.                                                   
003023                                                                          
003024*01  POST -COPY W46358 -PRE  HFEL-  -L.                                   
003025                                                                          
003030*01  POST -COPY W46357 -PRE  RFEL-  -L.                                   
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W4635800'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W46357-EOF-SW               PIC X       VALUE 'N'.                   
003803     88  END-OF-W46357                       VALUE 'J'.                   
003804                                                                          
003805 77  W46358-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W46358                       VALUE 'J'.                   
003900     EJECT                                                                
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004410 01  WS-IDPRODNR                 PIC 9(7).                                
004420                                                                          
004500     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202 01  IN-AREA-START               PIC X(24)   VALUE                        
006203                                 'IN-AREA-START  '.                       
006204     SKIP2                                                                
006205                                                                          
006206*01  AREA -COPY W46357     -PRE IN-                                       
006207     EJECT                                                                
006208 01  FEL-AREA-START              PIC X(24)   VALUE                        
006209                                 'FEL-AREA-START  '.                      
006210     SKIP2                                                                
006211                                                                          
006212*01  AREA -COPY W46358     -PRE INFEL-                                    
006213     EJECT                                                                
006214 01  EDI-AREA-START              PIC X(24)   VALUE                        
006215                                 'EDI-AREA-START  '.                      
006216     SKIP2                                                                
006217                                                                          
006218*01  AREA -COPY W46357     -PRE EDI-                                      
006219                                                                          
006220     EJECT                                                                
006221 01  FEL-AREA-START              PIC X(24)   VALUE                        
006222                                 'FEL-AREA-START  '.                      
006223     SKIP2                                                                
006224 01  FEL-AREA.                                                            
006225     03  FEL-AREA-0               PIC X(990).                             
006229*    03  FILLER -COPY W46358  -PRE HFEL-  -RED  FEL-AREA-0                
006230*    03  FILLER -COPY W46357  -PRE RFEL-  -RED  FEL-AREA-0                
006300     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007001     PERFORM S01-LAES-W46357                                              
007010     PERFORM S02-LAES-W46358                                              
007020     MOVE IN-IDPRODNR           TO WS-IDPRODNR                            
007100     PERFORM UNTIL END-OF-W46357                                          
007200      IF INFEL-IDPRODNR = WS-IDPRODNR AND                                 
007210         NOT END-OF-W46358                                                
007300        PERFORM B-FLYTTA-FEL-HDATA                                        
007400        PERFORM S12-SKRIV-HFEL                                            
007410                                                                          
007500        PERFORM UNTIL WS-IDPRODNR NOT = INFEL-IDPRODNR OR                 
007501                      END-OF-W46357                                       
007510         PERFORM C-FLYTTA-FEL-RDATA                                       
007600         PERFORM S13-SKRIV-RFEL                                           
007610         PERFORM S01-LAES-W46357                                          
007620         MOVE IN-IDPRODNR       TO WS-IDPRODNR                            
007630        END-PERFORM                                                       
007631                                                                          
007632        PERFORM S02-LAES-W46358                                           
007633      ELSE                                                                
007636       MOVE IN-AREA             TO EDI-AREA                               
007637       PERFORM S11-SKRIV-W46359                                           
007638       PERFORM S01-LAES-W46357                                            
007639       MOVE IN-IDPRODNR         TO WS-IDPRODNR                            
007640      END-IF                                                              
007900     END-PERFORM                                                          
008000                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008902     OPEN INPUT  W46357                                                   
008910                 W46358                                                   
009001                                                                          
009002     OPEN OUTPUT W46359                                                   
009010                 W46354                                                   
009100     SKIP2                                                                
009200     ACCEPT DAGENS-DATUM  FROM DATE                                       
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009510 B-FLYTTA-FEL-HDATA SECTION.                                              
009511* * * H = HUVUDDEL                                                        
009520     MOVE INFEL-IDPRODNR      TO HFEL-IDPRODNR                            
009521     MOVE INFEL-DAREGDAT      TO HFEL-DAREGDAT                            
009522     MOVE INFEL-IDSUPREF      TO HFEL-IDSUPREF                            
009523     MOVE INFEL-FELTEXT       TO HFEL-FELTEXT                             
009530     .                                                                    
009540     EJECT                                                                
009550 C-FLYTTA-FEL-RDATA SECTION.                                              
009560* * * R = RADDEL                                                          
009561*                                                                         
009562     MOVE IN-AREA             TO RFEL-W46357                              
009563*    MOVE IN-IDANSTNR         TO RFEL-IDANSTNR                            
009564*    MOVE IN-IDDISTR          TO RFEL-IDDISTR                             
009565*    MOVE IN-IDKUNDNR         TO RFEL-IDKUNDNR                            
009566*    MOVE IN-IDORDNR          TO RFEL-IDORDNR                             
009567*    MOVE IN-IDDC             TO RFEL-IDDC                                
009568*    MOVE IN-IDKOLLI          TO RFEL-IDKOLLI                             
009569*    MOVE IN-IDSUPREF         TO RFEL-IDSUPREF                            
009570*    MOVE IN-DASUPREF         TO RFEL-DASUPREF                            
009571*    MOVE IN-TISUPTID         TO RFEL-TISUPTID                            
009572*    MOVE IN-VKORDBTO-KOLLI   TO RFEL-VKORDBTO-KOLLI                      
009573*    MOVE IN-KDEMBTYP         TO RFEL-KDEMBTYP                            
009574*    MOVE IN-DIKOLLIL         TO RFEL-DIKOLLIL                            
009575*    MOVE IN-DIKOLLIB         TO RFEL-DIKOLLIB                            
009576*    MOVE IN-DIKOLLIH         TO RFEL-DIKOLLIH                            
009577*    MOVE IN-FLSLUT           TO RFEL-FLSLUT                              
009578*    MOVE IN-IDSNDNOD         TO RFEL-IDSNDNOD                            
009579*    MOVE IN-RAD              TO RFEL-RAD                                 
009580     .                                                                    
009590     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W46357                                                         
009702           W46358                                                         
009703           W46359                                                         
009710           W46354                                                         
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-LAES-W46357  SECTION.                                                
010003     READ W46357 INTO IN-AREA                                             
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN-AREA                                        
010006        SET END-OF-W46357 TO TRUE                                         
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'W46357'   TO POSTSUM-FDNAMN                                 
010010        MOVE 'W46358D1' TO POSTSUM-DDNAMN2                                
010013        MOVE 'RAET'     TO POSTSUM-TRANSTYP                               
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010016     .                                                                    
010017     EJECT                                                                
010018 S02-LAES-W46358  SECTION.                                                
010019     READ W46358 INTO INFEL-AREA                                          
010020     AT END                                                               
010021        MOVE HIGH-VALUE TO FEL-AREA                                       
010022        SET END-OF-W46358 TO TRUE                                         
010023                                                                          
010024     NOT AT END                                                           
010025        MOVE 'W46358'   TO POSTSUM-FDNAMN                                 
010026        MOVE 'W46358D2' TO POSTSUM-DDNAMN2                                
010029        MOVE 'FEL'      TO POSTSUM-TRANSTYP                               
010030        CALL POSTSUM USING POSTSUM-PARM                                   
010031     END-READ                                                             
010040     .                                                                    
010101     EJECT                                                                
010102 S11-SKRIV-W46359 SECTION.                                                
010103                                                                          
010104     WRITE EDI-POST FROM EDI-AREA                                         
010105                                                                          
010106     MOVE 'EDI'         TO POSTSUM-TRANSTYP                               
010107     MOVE 'W46359'      TO POSTSUM-FDNAMN                                 
010108     MOVE 'W46358D3'    TO POSTSUM-DDNAMN2                                
010109     CALL POSTSUM USING POSTSUM-PARM                                      
010110     .                                                                    
010111     EJECT                                                                
010112 S12-SKRIV-HFEL   SECTION.                                                
010113                                                                          
010114     WRITE HFEL-POST FROM FEL-AREA-0                                      
010115                                                                          
010116     MOVE 'HFEL'         TO POSTSUM-TRANSTYP                              
010117     MOVE 'W46354'      TO POSTSUM-FDNAMN                                 
010118     MOVE 'W46358D4'    TO POSTSUM-DDNAMN2                                
010119     CALL POSTSUM USING POSTSUM-PARM                                      
010120     .                                                                    
010300     EJECT                                                                
010310 S13-SKRIV-RFEL   SECTION.                                                
010320                                                                          
010330     WRITE RFEL-POST FROM FEL-AREA-0                                      
010340                                                                          
010350     MOVE 'RFEL'         TO POSTSUM-TRANSTYP                              
010360     MOVE 'W46354'      TO POSTSUM-FDNAMN                                 
010370     MOVE 'W46358D4'    TO POSTSUM-DDNAMN2                                
010380     CALL POSTSUM USING POSTSUM-PARM                                      
010390     .                                                                    
010391     EJECT                                                                
010400 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
