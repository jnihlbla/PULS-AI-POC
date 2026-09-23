000010                                                                          
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4769200.                                                
000300 AUTHOR.         STINAMOGREN.                                             
000400 DATE-WRITTEN.   06/10/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        KONTROLLERAR FIL MEDFAKTURERADE SATSORDER. SKRIVER FIL           
001000*        TILL INLVEVERANS MED SATSER FÖR INLÄGGNING.                      
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
002402*          --- FAKTURERADE SATORDER                                       
002403     SELECT W4765H                     ASSIGN TO W47692D1.                
002404     SKIP2                                                                
002405*          --- SATSER TILL INLVERANS                                      
002410     SELECT W47692                     ASSIGN TO W47692D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003001     SKIP2                                                                
003002 FD  W4765H                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006 01  INPOST.                                                              
003007*    03 -COPY W476SAT   -L.                                               
003011                                                                          
003014 FD  W47692                                                               
003015     RECORDING       F                                                    
003016     BLOCK CONTAINS  0.                                                   
003017                                                                          
003020*01  POST -COPY W211R31 -PRE  SORD-  -L.                                  
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W4769200'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W4765H-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W4765H                       VALUE 'J'.                   
003820                                                                          
003860 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
003870     88 SKRIV-JA                             VALUE 'J'.                   
003871     88 SKRIV-NEJ                            VALUE 'N'.                   
003880                                                                          
003890 77  BEHANDLA-SW                 PIC X       VALUE 'N'.                   
003891     88  BEHANDLA                            VALUE 'J'.                   
003893                                                                          
003900     EJECT                                                                
003910                                                                          
003911 01  FILLER                       PIC X(16)  VALUE 'ARBETSFALT'.          
003920 01  ARBETSFALT.                                                          
003921   03  WS-BEGMT.                                                          
003922     05  WS-BEGMT-IDARTNR         PIC  9(08).                             
003923     05  FILLER                   PIC  X(27).                             
003924     05  WS-BEGMT-KVORDRAD        PIC  9(03).                             
003925     05  WS-BEGMT-KVBEART         PIC  9(06).                             
003926     05  FILLER                   PIC  X(26).                             
003927                                                                          
003928 01  WS-IDFAKT                    PIC  S9(7) VALUE ZERO  COMP-3.          
003929 01  WS-IDARTNR                   PIC  S9(9) VALUE ZERO  COMP-3.          
003930 01  WS-IDKUNDRF                  PIC  X(10) VALUE SPACE.                 
003931                                                                          
003932                                                                          
003933 01  FILLER                       PIC X(16)  VALUE 'KONSTANTER'.          
003934 01  KONSTANTER.                                                          
003935                                                                          
003936     03 WC-SATS-IDPTYP            PIC  X(03)  VALUE '310'.                
003937     03 WC-SATS-KDSORT2           PIC S9(03)  VALUE +04    COMP-3.        
003938     03 WC-SATS-IDLEVNR           PIC  X(05)  VALUE '1002 '.              
003939     03 WC-SATS-KDRT              PIC S9(03)  VALUE +03    COMP-3.        
003940                                                                          
003950                                                                          
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
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
006205 01  IN-AREA.                                                             
006206     03  IN-AREA-KEY-AREA.                                                
006207*        05 -COPY W476SAT  -PRE SAT-                                      
006216                                                                          
006217     EJECT                                                                
006218 01  SORD-AREA-START             PIC X(24)   VALUE                        
006219                                 'SORD-AREA-START  '.                     
006220     SKIP2                                                                
006221                                                                          
006230*01  AREA -COPY W211R31     -PRE SORD-                                    
006300     EJECT                                                                
006310                                                                          
006320                                                                          
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-LAES-W4765H                                              
007100     PERFORM UNTIL END-OF-W4765H                                          
007324        IF SAT-KDORDKL = +5 AND                                           
007325           SAT-KDSOFT = +0                                                
007328           PERFORM B-BEHANDLA-FAKT-RAD                                    
007329           IF SKRIV-JA                                                    
007512             PERFORM S11-SKRIV-W47692                                     
007513             MOVE NEJ             TO SKRIV-SW                             
007514           END-IF                                                         
007520        END-IF                                                            
007602        PERFORM S01-LAES-W4765H                                           
007603     END-PERFORM                                                          
007604                                                                          
007609     PERFORM Z-FINIT                                                      
007610                                                                          
007611     MOVE ZERO TO RETURN-CODE                                             
007612     GOBACK                                                               
007613     .                                                                    
007614     EJECT                                                                
007615                                                                          
007616                                                                          
007617 B-BEHANDLA-FAKT-RAD SECTION.                                             
007618                                                                          
007619     MOVE WC-SATS-IDPTYP          TO SORD-IDPTYP                          
007620     MOVE WC-SATS-KDSORT2         TO SORD-KDSORT2                         
007621     MOVE WC-SATS-IDLEVNR         TO SORD-IDLEVNR-INL                     
007622     MOVE WC-SATS-KDRT            TO SORD-KDRT                            
007624     MOVE SAT-IDARTNR             TO SORD-IDARTNR                         
007625     MOVE SAT-KVBEART             TO SORD-KVAVIS                          
007626     MOVE SAT-TIFAKT              TO SORD-TIAVSDAT                        
007627     MOVE ZERO                    TO SORD-IDORDNR                         
007628                                     SORD-IDRADNR                         
007629                                     SORD-IDPLFORM                        
007639                                                                          
007641                                                                          
007642     MOVE SAT-IDKUNDRF(1:5)       TO SORD-IDAVINR                         
007643     MOVE SAT-IDKONTO             TO SORD-IDKONTO                         
007644     MOVE SAT-IDANALYS            TO SORD-IDANALYS                        
007645     MOVE SAT-IDKST               TO SORD-IDKST                           
007646     IF SKRIV-NEJ                                                         
007647       IF WS-IDFAKT   = SAT-IDFAKT  AND                                   
007648          WS-IDARTNR  = SAT-IDARTNR AND                                   
007649          WS-IDKUNDRF = SAT-IDKUNDRF                                      
007650         CONTINUE                                                         
007651       ELSE                                                               
007652         MOVE SAT-IDFAKT          TO WS-IDFAKT                            
007653         MOVE SAT-IDARTNR         TO WS-IDARTNR                           
007654         MOVE SAT-IDKUNDRF        TO WS-IDKUNDRF                          
007655         MOVE JA                  TO SKRIV-SW                             
007656       END-IF                                                             
007657     END-IF                                                               
007658     .                                                                    
007659     EJECT                                                                
007660                                                                          
007670                                                                          
008800 A-INIT SECTION.                                                          
008901                                                                          
008910     OPEN INPUT  W4765H                                                   
009001                                                                          
009010     OPEN OUTPUT W47692                                                   
009100     SKIP2                                                                
009200     ACCEPT DAGENS-DATUM   FROM DATE                                      
009310     MOVE IDPGM            TO POSTSUM-PROGNAMN                            
009400     .                                                                    
009500     EJECT                                                                
009520                                                                          
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W4765H                                                         
009710           W47692                                                         
009801     SKIP2                                                                
009802     MOVE 'S'              TO POSTSUM-OPKOD                               
009810     CALL POSTSUM          USING POSTSUM-PARM                             
009900     .                                                                    
010001     EJECT                                                                
010002                                                                          
010003                                                                          
010004 S01-LAES-W4765H  SECTION.                                                
010005     READ W4765H INTO IN-AREA                                             
010006     AT END                                                               
010007        MOVE HIGH-VALUE    TO IN-AREA                                     
010008        SET END-OF-W4765H  TO TRUE                                        
010009                                                                          
010010     NOT AT END                                                           
010011        MOVE 'W4765H'      TO POSTSUM-FDNAMN                              
010012        MOVE 'W47692D1'    TO POSTSUM-DDNAMN2                             
010015        MOVE 'SAT'         TO POSTSUM-TRANSTYP                            
010016        CALL POSTSUM       USING POSTSUM-PARM                             
010017     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010102                                                                          
010103                                                                          
010104 S11-SKRIV-W47692 SECTION.                                                
010105                                                                          
010106     WRITE SORD-POST  FROM SORD-AREA                                      
010107                                                                          
010108     MOVE SORD-IDPTYP TO POSTSUM-TRANSTYP                                 
010109     MOVE 'W47692'    TO POSTSUM-FDNAMN                                   
010110     MOVE 'W47692D2'  TO POSTSUM-DDNAMN2                                  
010111     MOVE '310'       TO POSTSUM-TRANSTYP                                 
010112     CALL POSTSUM     USING POSTSUM-PARM                                  
010120     .                                                                    
010300     EJECT                                                                
010400 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S'         TO POSTSUM-OPKOD                                    
010610     CALL POSTSUM     USING POSTSUM-PARM                                  
010700     CALL ABEND       USING RKOD-ABEND                                    
010800     .                                                                    
