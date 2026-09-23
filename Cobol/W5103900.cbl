000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5103900.                                                
000400 AUTHOR.         KARL JOHAN HANSSON.                                      
000500 DATE-WRITTEN.   96/09/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER NER WDR8 BASEN OCH SKAPAR EN RENSNINGSFIL SAMT             
001000*        ETT ANTAL TRANSAKTIONSFILER.                                     
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLFILB (WDR8)                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- FIL MED TRANSAR SOM SKA RENSAS FRÅN WDR8                   
002500     SELECT W51039                     ASSIGN TO W51039D1.                
002600     SKIP2                                                                
002700*          --- LABTRANSAR                                                 
002800     SELECT W51040                     ASSIGN TO W51039D2.                
002900     SKIP2                                                                
003000*          --- RETURSYSTEMETS TRANSAR                                     
003100     SELECT W5103A                     ASSIGN TO W51039D3.                
003200     EJECT                                                                
003300*          --- WDR8 TILL KINA                                             
003400     SELECT W51041                     ASSIGN TO W51039D4.                
003500     EJECT                                                                
003510*          --- WDR8 TILL KINA 102-121                                     
003520     SELECT W5104A                     ASSIGN TO W51039D5.                
003530     EJECT                                                                
003540*          --- WDR8 TILL INDIEN                                           
003550     SELECT W51042                     ASSIGN TO W51039D6.                
003560     EJECT                                                                
003570*          --- WDR8 TILL INDIEN 102-121                                   
003580     SELECT W5104B                     ASSIGN TO W51039D7.                
003590     EJECT                                                                
003591*          --- WDR8 TILL USA                                              
003592     SELECT W51043                     ASSIGN TO W51039D8.                
003593     EJECT                                                                
003594*          --- WDR8 TILL USA 102-121                                      
003595     SELECT W5104C                     ASSIGN TO W51039D9.                
003596*          --- WDR8 TILL XX                                               
003597     SELECT W51047                     ASSIGN TO W51039DA.                
003598     EJECT                                                                
003599*          --- WDR8 TILL XX 102-121                                       
003600     SELECT W5104D                     ASSIGN TO W51039DB.                
003601     EJECT                                                                
003610 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W51039                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY WDR801 -PRE  UTRENS-  -L.                                 
004500     SKIP3                                                                
004600 FD  W51040                                                               
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W510A03 -PRE  UTA03-  -L.                                 
005100*01  POST -COPY W510A06 -PRE  UTA06-  -L.                                 
005200*01  POST -COPY W510A08 -PRE  UTA08-  -L.                                 
005300*01  POST -COPY W510A10 -PRE  UTA10-  -L.                                 
005400*01  POST -COPY W510A11 -PRE  UTA11-  -L.                                 
005500*01  POST -COPY W510A13 -PRE  UTA13-  -L.                                 
005600*01  POST -COPY W510A14 -PRE  UTA14-  -L.                                 
005700*01  POST -COPY W510A15 -PRE  UTA15-  -L.                                 
005800*01  POST -COPY W510A16 -PRE  UTA16-  -L.                                 
005900*01  POST -COPY W510A17 -PRE  UTA17-  -L.                                 
006000*01  POST -COPY W510A18 -PRE  UTA18-  -L.                                 
006100*01  POST -COPY W510A19 -PRE  UTA19-  -L.                                 
006200     SKIP3                                                                
006300 FD  W5103A                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700*01  POST -COPY W51080  -PRE  UTLEVA1- -L.                                
006800     EJECT                                                                
006900                                                                          
007000 FD  W51041                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY W57060 -PRE  SAPCN-  -L.                                  
007500     SKIP3                                                                
007510 FD  W5104A                                                               
007520     RECORDING       F                                                    
007530     BLOCK CONTAINS  0.                                                   
007540                                                                          
007550*01  POST -COPY W57060 -PRE  102CN-  -L.                                  
007560     SKIP3                                                                
007570 FD  W51042                                                               
007580     RECORDING       F                                                    
007590     BLOCK CONTAINS  0.                                                   
007591                                                                          
007592*01  POST -COPY W57060 -PRE  SAPIN-  -L.                                  
007593     SKIP3                                                                
007594 FD  W5104B                                                               
007595     RECORDING       F                                                    
007596     BLOCK CONTAINS  0.                                                   
007597                                                                          
007598*01  POST -COPY W57060 -PRE  102IN-  -L.                                  
007599     SKIP3                                                                
007600 FD  W51043                                                               
007601     RECORDING       F                                                    
007602     BLOCK CONTAINS  0.                                                   
007603                                                                          
007604*01  POST -COPY W57060 -PRE  SAPUS-  -L.                                  
007605     SKIP3                                                                
007606 FD  W5104C                                                               
007607     RECORDING       F                                                    
007608     BLOCK CONTAINS  0.                                                   
007609                                                                          
007610*01  POST -COPY W57060 -PRE  102US-  -L.                                  
007611     SKIP3                                                                
007612 FD  W51047                                                               
007613     RECORDING       F                                                    
007614     BLOCK CONTAINS  0.                                                   
007615                                                                          
007616*01  POST -COPY W57060 -PRE  SAPXX- -L.                                   
007617     SKIP3                                                                
007618 FD  W5104D                                                               
007619     RECORDING       F                                                    
007620     BLOCK CONTAINS  0.                                                   
007621                                                                          
007622*01  POST -COPY W57060 -PRE  102XX-  -L.                                  
007623     SKIP3                                                                
007630 WORKING-STORAGE SECTION.                                                 
007700                                                                          
007800*    -- CHECKED BY WY2000                                                 
007900 77  IDPGM                       PIC X(8)    VALUE 'W5103900'.            
008000 77  JA                          PIC X       VALUE 'J'.                   
008100 77  NEJ                         PIC X       VALUE 'N'.                   
008200                                                                          
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008800     SKIP2                                                                
008900*    --- PARAMETRAR TILL ABEND                                            
009000                                                                          
009100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009300     SKIP2                                                                
009400 01  FELTEXT.                                                             
009500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL POSTSUM                                          
009900*                                                                         
010000*01  -COPY W0005   -PRE  POSTSUM-                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)  VALUE 'LEVA1-AREA'.           
010300                                                                          
010400*01  AREA -COPY W51080      -PRE LEVA1-                                   
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)  VALUE 'LAB-AREA '.            
010700                                                                          
010800 01  LAB-AREA                    PIC X(300).                              
010900*01  AREA -COPY W510A03 -RED LAB-AREA -PRE A03-                           
011000*01  AREA -COPY W510A06 -RED LAB-AREA -PRE A06-                           
011100*01  AREA -COPY W510A08 -RED LAB-AREA -PRE A08-                           
011200*01  AREA -COPY W510A10 -RED LAB-AREA -PRE A10-                           
011300*01  AREA -COPY W510A11 -RED LAB-AREA -PRE A11-                           
011400*01  AREA -COPY W510A13 -RED LAB-AREA -PRE A13-                           
011500*01  AREA -COPY W510A14 -RED LAB-AREA -PRE A14-                           
011600*01  AREA -COPY W510A15 -RED LAB-AREA -PRE A15-                           
011700*01  AREA -COPY W510A16 -RED LAB-AREA -PRE A16-                           
011800*01  AREA -COPY W510A17 -RED LAB-AREA -PRE A17-                           
011900*01  AREA -COPY W510A18 -RED LAB-AREA -PRE A18-                           
012000*01  AREA -COPY W510A19 -RED LAB-AREA -PRE A19-                           
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)  VALUE 'RENS-AREA'.            
012300                                                                          
012400*01  AREA -COPY WDR801     -PRE UTRENS-                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'SAPCN-AREA'.           
012610                                                                          
012620*01  AREA -COPY W57060     -PRE SAPCN-                                    
012630     EJECT                                                                
012640 01  FILLER                      PIC X(16)  VALUE '102CN-AREA'.           
012650                                                                          
012660*01  AREA -COPY W57060     -PRE 102CN-                                    
012670     EJECT                                                                
012680 01  FILLER                      PIC X(16)  VALUE 'SAPIN-AREA'.           
012690                                                                          
012691*01  AREA -COPY W57060     -PRE SAPIN-                                    
012692     EJECT                                                                
012693 01  FILLER                      PIC X(16)  VALUE '102IN-AREA'.           
012694                                                                          
012695*01  AREA -COPY W57060     -PRE 102IN-                                    
012696     EJECT                                                                
012697                                                                          
012698 01  FILLER                      PIC X(16)  VALUE 'SAPUS-AREA'.           
012699                                                                          
012700*01  AREA -COPY W57060     -PRE SAPUS-                                    
012701     EJECT                                                                
012702 01  FILLER                      PIC X(16)  VALUE '102US-AREA'.           
012703                                                                          
012704*01  AREA -COPY W57060     -PRE 102US-                                    
012705     EJECT                                                                
012706 01  FILLER                      PIC X(16)  VALUE 'SAPXX-AREA'.           
012707                                                                          
012708*01  AREA -COPY W57060     -PRE SAPXX-                                    
012709     EJECT                                                                
012710 01  FILLER                      PIC X(16)  VALUE '102XX-AREA'.           
012711                                                                          
012712*01  AREA -COPY W57060     -PRE 102XX-                                    
012713     EJECT                                                                
012720*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  NYCKLAR-TILL-DLI.                                                    
013100     03  W-WDR801KY-X.                                                    
013200         05  W-WDR801KY          PIC X(27)    VALUE SPACE.                
013300     SKIP2                                                                
013400*    --- STATUS-KOD FRÅN IMS                                              
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FINNS                       VALUE '  '.                  
013700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013800     SKIP2                                                                
013900 01  GODK-STATUSKODER.                                                    
014000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014100     SKIP3                                                                
014200 01  SSA1                        PIC X(64).                               
014300     EJECT                                                                
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014600     EJECT                                                                
014700*    ---  DLI INPUT-OUTPUT AREA                                           
014800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014900     SKIP3                                                                
015000 01  DLI-IO-AREA.                                                         
015100     03  WLFILB01.                                                        
015200*      05   -COPY WDR801   -PRE FILB-                                     
015210*        07 -COPY W510EKHA -PRE FILB- -RED FILB-FIL-WDR801-DATA           
015300     EJECT                                                                
015400 LINKAGE SECTION.                                                         
015500                                                                          
015600     EJECT                                                                
015700*01  -COPY W0008  -PRE FILB-                                              
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000 PROCEDURE DIVISION  USING FILB-PCB.                                      
016100 MAIN SECTION.                                                            
016200     ENTRY 'DLITCBL' USING FILB-PCB.                                      
016300                                                                          
016400                                                                          
016500     PERFORM A-INIT                                                       
016600     PERFORM IMS-GET-WDR8                                                 
016700     PERFORM UNTIL SEGMENT-SLUT                                           
016800                                                                          
016801**** MUST EXIST AN EMPTY POST IN DATABASE                                 
016802        IF FILB-FIL-IDCPYTXT(1:2) = 'W0'                                  
016803          CONTINUE                                                        
016804        ELSE                                                              
016805          MOVE FILB-FIL-WDR801 TO UTRENS-AREA                             
016806          PERFORM S11-SKRIV-W51039-RENS                                   
016807                                                                          
016810          IF FILB-FIL-IDCPYTXT(1:2) = 'W5'                                
016811            PERFORM B-PROCESS                                             
016812          ELSE                                                            
016813            IF FILB-FIL-IDCPYTXT(8:1) = 'B'                               
016814              PERFORM S19-SKRIV-W5104D                                    
016815            ELSE                                                          
016816              PERFORM S19-SKRIV-W51047                                    
016818            END-IF                                                        
016819          END-IF                                                          
016820        END-IF                                                            
016821                                                                          
016822        PERFORM IMS-GET-WDR8                                              
016823     END-PERFORM                                                          
016830*                                                                         
017210     PERFORM Z-FINIT                                                      
017220                                                                          
017230     MOVE ZERO TO RETURN-CODE                                             
017240     GOBACK                                                               
017250     .                                                                    
017260     EJECT                                                                
017300                                                                          
018700                                                                          
019400 A-INIT SECTION.                                                          
019500                                                                          
019600     OPEN OUTPUT W51039                                                   
019700                 W5103A                                                   
019800                 W51040                                                   
019810                 W51041                                                   
019820                 W5104A                                                   
019830                 W51042                                                   
019840                 W5104B                                                   
019850                 W51043                                                   
019860                 W5104C                                                   
019870                 W51047                                                   
019880                 W5104D                                                   
019900                                                                          
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020100     .                                                                    
020200     SKIP2                                                                
020210 B-PROCESS SECTION.                                                       
020220                                                                          
020230     EVALUATE FILB-FIL-IDCPYTXT                                           
020240***** SAP MM SVERIGE FILERNA                                              
020250       WHEN 'W51080  '                                                    
020260         MOVE FILB-FIL-WDR801-DATA   TO LEVA1-AREA                        
020270         PERFORM S18-SKRIV-W5103A-LEVA1                                   
020280***** KINA FILERNA                                                        
020290       WHEN 'W510EKHA'                                                    
020291         PERFORM S19-SKRIV-W51041                                         
020292       WHEN 'W570EKHA'                                                    
020293         PERFORM S19-SKRIV-W51041                                         
020294       WHEN 'W570EKFA'                                                    
020295         PERFORM S19-SKRIV-W51041                                         
020296       WHEN 'W570EKHB'                                                    
020297         PERFORM S19-SKRIV-W5104A                                         
020298***** INDIEN FILERNA                                                      
020299       WHEN 'W515EKHA'                                                    
020300         PERFORM S19-SKRIV-W51042                                         
020301       WHEN 'W515EKFA'                                                    
020302         PERFORM S19-SKRIV-W51042                                         
020303       WHEN 'W515EKHB'                                                    
020304         PERFORM S19-SKRIV-W5104B                                         
020305***** USA FILERNA                                                         
020306       WHEN 'W561EKHA'                                                    
020307         PERFORM S19-SKRIV-W51043                                         
020308       WHEN 'W561EKFA'                                                    
020309         PERFORM S19-SKRIV-W51043                                         
020310       WHEN 'W561EKHB'                                                    
020311         PERFORM S19-SKRIV-W5104C                                         
020312       WHEN OTHER                                                         
020313***** LAB FILERNA                                                         
020314         MOVE FILB-FIL-WDR801-DATA   TO LAB-AREA                          
020315         PERFORM S12-SKRIV-W51040-LAB                                     
020316     END-EVALUATE                                                         
020317     .                                                                    
020318     EJECT                                                                
020319                                                                          
020320                                                                          
020330 Z-FINIT SECTION.                                                         
020400                                                                          
020500     CLOSE W51039                                                         
020600           W5103A                                                         
020700           W51040                                                         
020710           W51041                                                         
020720           W5104A                                                         
020730           W51042                                                         
020740           W5104B                                                         
020750           W51043                                                         
020760           W5104C                                                         
020770           W51047                                                         
020780           W5104D                                                         
020800                                                                          
020900     MOVE 'S' TO POSTSUM-OPKOD                                            
021000     CALL POSTSUM USING POSTSUM-PARM                                      
021100     .                                                                    
021200     EJECT                                                                
021300 S11-SKRIV-W51039-RENS SECTION.                                           
021400                                                                          
021500     WRITE UTRENS-POST FROM UTRENS-AREA                                   
021600                                                                          
021700     MOVE ' R8'         TO POSTSUM-TRANSTYP                               
021800     MOVE 'W51039'      TO POSTSUM-FDNAMN                                 
021900     MOVE 'W51039D1'    TO POSTSUM-DDNAMN2                                
022000     CALL POSTSUM USING POSTSUM-PARM                                      
022100     .                                                                    
022200     EJECT                                                                
022300 S12-SKRIV-W51040-LAB  SECTION.                                           
022400                                                                          
022500     EVALUATE A03-IDPTYP                                                  
022600      WHEN 'A03' WRITE UTA03-POST FROM A03-AREA                           
022700      WHEN 'A06' WRITE UTA06-POST FROM A06-AREA                           
022800      WHEN 'L06' WRITE UTA06-POST FROM A06-AREA                           
022900      WHEN 'LX6' WRITE UTA06-POST FROM A06-AREA                           
023000      WHEN 'A08' WRITE UTA08-POST FROM A08-AREA                           
023100      WHEN 'A10' WRITE UTA10-POST FROM A10-AREA                           
023200      WHEN 'A11' WRITE UTA11-POST FROM A11-AREA                           
023300      WHEN 'L11' WRITE UTA11-POST FROM A11-AREA                           
023400      WHEN 'A13' WRITE UTA13-POST FROM A13-AREA                           
023500      WHEN 'A14' WRITE UTA14-POST FROM A14-AREA                           
023600      WHEN 'A15' WRITE UTA15-POST FROM A15-AREA                           
023700      WHEN 'A16' WRITE UTA16-POST FROM A16-AREA                           
023800      WHEN 'A17' WRITE UTA17-POST FROM A17-AREA                           
023900      WHEN 'A18' WRITE UTA18-POST FROM A18-AREA                           
024000      WHEN 'A19' WRITE UTA19-POST FROM A19-AREA                           
024100     END-EVALUATE                                                         
024200                                                                          
024300     MOVE A03-IDPTYP    TO POSTSUM-TRANSTYP                               
024400     MOVE 'W51040'      TO POSTSUM-FDNAMN                                 
024500     MOVE 'W51039D2'    TO POSTSUM-DDNAMN2                                
024600     CALL POSTSUM USING POSTSUM-PARM                                      
024700     .                                                                    
024800     SKIP2                                                                
024810                                                                          
024900 S18-SKRIV-W5103A-LEVA1 SECTION.                                          
025150     WRITE UTLEVA1-POST FROM LEVA1-AREA                                   
025200                                                                          
025300     MOVE 'LEV'         TO POSTSUM-TRANSTYP                               
025400     MOVE 'W5103A'      TO POSTSUM-FDNAMN                                 
025500     MOVE 'W51039D3'    TO POSTSUM-DDNAMN2                                
025600     CALL POSTSUM USING POSTSUM-PARM                                      
025700     .                                                                    
025800     EJECT                                                                
025801                                                                          
025810 S19-SKRIV-W51041       SECTION.                                          
025820     MOVE FILB-FIL-IDPGM       TO SAPCN-EKHT-IDPGM                        
025821     MOVE FILB-FIL-TIREGDAT    TO SAPCN-EKHT-TIREGDAT                     
025822     MOVE FILB-FIL-TIKLOCK     TO SAPCN-EKHT-TIKLOCK                      
025823     MOVE FILB-FIL-IDSEKVNR    TO SAPCN-EKHT-IDSEKVNR                     
025824     MOVE FILB-FIL-CT-IDSYSTEM TO SAPCN-EKHT-CT-IDSYSTEM                  
025825     MOVE FILB-FIL-CT-IDPTYP  TO SAPCN-EKHT-CT-IDPTYP                     
025826     MOVE FILB-FIL-CT-IDVTYP  TO SAPCN-EKHT-CT-IDVTYP                     
025827     MOVE FILB-EKH-BEVAT      TO SAPCN-EKHT-BEVAT                         
025828     MOVE FILB-EKH-DAVERDAT   TO SAPCN-EKHT-DAVERDAT                      
025829     MOVE FILB-EKH-FLLSBOK    TO SAPCN-EKHT-FLLSBOK                       
025830     MOVE FILB-EKH-IDANALYS   TO SAPCN-EKHT-IDANALYS                      
025831     MOVE FILB-EKH-IDARTNR    TO SAPCN-EKHT-IDARTNR                       
025832     MOVE FILB-EKH-IDDC-SEND  TO SAPCN-EKHT-IDDC-SEND                     
025833     MOVE FILB-EKH-IDDC-REC   TO SAPCN-EKHT-IDDC-REC                      
025834     MOVE FILB-EKH-IDDISTR    TO SAPCN-EKHT-IDDISTR                       
025835     MOVE FILB-EKH-IDKONTO    TO SAPCN-EKHT-IDKONTO                       
025836     MOVE FILB-EKH-IDKST      TO SAPCN-EKHT-IDKST                         
025837     MOVE FILB-EKH-IDKUNDNR   TO SAPCN-EKHT-IDKUNDNR                      
025838     MOVE FILB-EKH-IDTRANS    TO SAPCN-EKHT-IDTRANS                       
025839     MOVE FILB-EKH-IDVERGL    TO SAPCN-EKHT-IDVERGL                       
025840     MOVE FILB-EKH-KDANMORS   TO SAPCN-EKHT-KDANMORS                      
025841     MOVE FILB-EKH-KDEKHHT    TO SAPCN-EKHT-KDEKHHT                       
025842     MOVE FILB-EKH-KDEKSHT    TO SAPCN-EKHT-KDEKSHT                       
025843     MOVE FILB-EKH-KDEKNIVA   TO SAPCN-EKHT-KDEKNIVA                      
025844     MOVE FILB-EKH-KDFRAKT    TO SAPCN-EKHT-KDFRAKT                       
025845     MOVE FILB-EKH-KDPRODSL   TO SAPCN-EKHT-KDPRODSL                      
025846     MOVE FILB-EKH-KDPSLLOC   TO SAPCN-EKHT-KDPSLLOC                      
025847     MOVE FILB-EKH-KDVALISO   TO SAPCN-EKHT-KDVALISO                      
025848     MOVE FILB-EKH-KVANTAL    TO SAPCN-EKHT-KVANTAL                       
025849     MOVE FILB-EKH-PRARTNTO   TO SAPCN-EKHT-PRARTNTO                      
025850     MOVE FILB-EKH-PRARTSJK   TO SAPCN-EKHT-PRARTSJK                      
025851     MOVE FILB-EKH-PRARTSTD   TO SAPCN-EKHT-PRARTSTD                      
025852     MOVE FILB-EKH-PRDIRLON   TO SAPCN-EKHT-PRDIRLON                      
025853     MOVE FILB-EKH-PRDMTRL    TO SAPCN-EKHT-PRDMTRL                       
025854     MOVE FILB-EKH-PRINK      TO SAPCN-EKHT-PRINK                         
025855     MOVE FILB-EKH-PRKURS     TO SAPCN-EKHT-PRKURS                        
025856     MOVE FILB-EKH-PRLANDCO   TO SAPCN-EKHT-PRLANDCO                      
025857     MOVE FILB-EKH-PROVRPAL   TO SAPCN-EKHT-PROVRPAL                      
025858     MOVE FILB-EKH-SUBEL      TO SAPCN-EKHT-SUBEL                         
025859     MOVE FILB-EKH-SUVAT      TO SAPCN-EKHT-SUVAT                         
025860     MOVE FILB-EKH-DAAVIDAT   TO SAPCN-EKHT-DAAVIDAT                      
025861     MOVE FILB-EKH-IDAVINR    TO SAPCN-EKHT-IDAVINR                       
025862     MOVE FILB-EKH-IDLEVNR    TO SAPCN-EKHT-IDLEVNR                       
025863     MOVE FILB-EKH-KDAVVTYP   TO SAPCN-EKHT-KDAVVTYP                      
025864     MOVE FILB-EKH-KDRT       TO SAPCN-EKHT-KDRT                          
025865     MOVE FILB-EKH-KVANTMOT   TO SAPCN-EKHT-KVANTMOT                      
025866     MOVE FILB-EKH-KVAVIS     TO SAPCN-EKHT-KVAVIS                        
025867     MOVE FILB-EKH-KDSORT     TO SAPCN-EKHT-KDSORT                        
025868     MOVE FILB-EKH-KDTRADP    TO SAPCN-EKHT-KDTRADP                       
025869     MOVE FILB-EKH-FLOVRLEV   TO SAPCN-EKHT-FLOVRLEV                      
025870     IF FILB-EKH-IDORDNR5 NOT NUMERIC                                     
025871       MOVE ZERO              TO SAPCN-EKHT-IDORDNR5                      
025872     ELSE                                                                 
025873       MOVE FILB-EKH-IDORDNR5 TO SAPCN-EKHT-IDORDNR5                      
025874     END-IF                                                               
025875     MOVE FILB-EKH-IDUSER     TO SAPCN-EKHT-IDUSER                        
025876     IF FILB-EKH-PRHEMTAG NOT NUMERIC                                     
025877       MOVE ZERO              TO SAPCN-EKHT-PRHEMTAG                      
025878     ELSE                                                                 
025879       MOVE FILB-EKH-PRHEMTAG TO SAPCN-EKHT-PRHEMTAG                      
025880     END-IF                                                               
025881     MOVE FILB-EKH-FLDCET     TO SAPCN-EKHT-FLDCET                        
025882     MOVE FILB-EKH-IDKUNDRF   TO SAPCN-EKHT-IDKUNDRF                      
025883     MOVE FILB-EKH-IDFAKT-EXP TO SAPCN-EKHT-IDFAKT-EXP                    
025885                                                                          
025886     WRITE SAPCN-POST   FROM SAPCN-AREA                                   
025887                                                                          
025888     MOVE 'SAP'         TO POSTSUM-TRANSTYP                               
025889     MOVE 'W51041'      TO POSTSUM-FDNAMN                                 
025890     MOVE 'W51039D4'    TO POSTSUM-DDNAMN2                                
025891     CALL POSTSUM USING POSTSUM-PARM                                      
025892     .                                                                    
025893     EJECT                                                                
025894                                                                          
025895 S19-SKRIV-W5104A       SECTION.                                          
025896     MOVE FILB-FIL-IDPGM       TO 102CN-EKHT-IDPGM                        
025897     MOVE FILB-FIL-TIREGDAT    TO 102CN-EKHT-TIREGDAT                     
025898     MOVE FILB-FIL-TIKLOCK     TO 102CN-EKHT-TIKLOCK                      
025899     MOVE FILB-FIL-IDSEKVNR    TO 102CN-EKHT-IDSEKVNR                     
025900     MOVE FILB-FIL-CT-IDSYSTEM TO 102CN-EKHT-CT-IDSYSTEM                  
025901     MOVE FILB-FIL-CT-IDPTYP  TO 102CN-EKHT-CT-IDPTYP                     
025902     MOVE FILB-FIL-CT-IDVTYP  TO 102CN-EKHT-CT-IDVTYP                     
025903     MOVE FILB-EKH-BEVAT      TO 102CN-EKHT-BEVAT                         
025904     MOVE FILB-EKH-DAVERDAT   TO 102CN-EKHT-DAVERDAT                      
025905     MOVE FILB-EKH-FLLSBOK    TO 102CN-EKHT-FLLSBOK                       
025906     MOVE FILB-EKH-IDANALYS   TO 102CN-EKHT-IDANALYS                      
025907     MOVE FILB-EKH-IDARTNR    TO 102CN-EKHT-IDARTNR                       
025908     MOVE FILB-EKH-IDDC-SEND  TO 102CN-EKHT-IDDC-SEND                     
025909     MOVE FILB-EKH-IDDC-REC   TO 102CN-EKHT-IDDC-REC                      
025910     MOVE FILB-EKH-IDDISTR    TO 102CN-EKHT-IDDISTR                       
025911     MOVE FILB-EKH-IDKONTO    TO 102CN-EKHT-IDKONTO                       
025912     MOVE FILB-EKH-IDKST      TO 102CN-EKHT-IDKST                         
025913     MOVE FILB-EKH-IDKUNDNR   TO 102CN-EKHT-IDKUNDNR                      
025914     MOVE FILB-EKH-IDTRANS    TO 102CN-EKHT-IDTRANS                       
025915     MOVE FILB-EKH-IDVERGL    TO 102CN-EKHT-IDVERGL                       
025916     MOVE FILB-EKH-KDANMORS   TO 102CN-EKHT-KDANMORS                      
025917     MOVE FILB-EKH-KDEKHHT    TO 102CN-EKHT-KDEKHHT                       
025918     MOVE FILB-EKH-KDEKSHT    TO 102CN-EKHT-KDEKSHT                       
025919     MOVE FILB-EKH-KDEKNIVA   TO 102CN-EKHT-KDEKNIVA                      
025920     MOVE FILB-EKH-KDFRAKT    TO 102CN-EKHT-KDFRAKT                       
025921     MOVE FILB-EKH-KDPRODSL   TO 102CN-EKHT-KDPRODSL                      
025922     MOVE FILB-EKH-KDPSLLOC   TO 102CN-EKHT-KDPSLLOC                      
025923     MOVE FILB-EKH-KDVALISO   TO 102CN-EKHT-KDVALISO                      
025924     MOVE FILB-EKH-KVANTAL    TO 102CN-EKHT-KVANTAL                       
025925     MOVE FILB-EKH-PRARTNTO   TO 102CN-EKHT-PRARTNTO                      
025926     MOVE FILB-EKH-PRARTSJK   TO 102CN-EKHT-PRARTSJK                      
025927     MOVE FILB-EKH-PRARTSTD   TO 102CN-EKHT-PRARTSTD                      
025928     MOVE FILB-EKH-PRDIRLON   TO 102CN-EKHT-PRDIRLON                      
025929     MOVE FILB-EKH-PRDMTRL    TO 102CN-EKHT-PRDMTRL                       
025930     MOVE FILB-EKH-PRINK      TO 102CN-EKHT-PRINK                         
025931     MOVE FILB-EKH-PRKURS     TO 102CN-EKHT-PRKURS                        
025932     MOVE FILB-EKH-PRLANDCO   TO 102CN-EKHT-PRLANDCO                      
025933     MOVE FILB-EKH-PROVRPAL   TO 102CN-EKHT-PROVRPAL                      
025934     MOVE FILB-EKH-SUBEL      TO 102CN-EKHT-SUBEL                         
025935     MOVE FILB-EKH-SUVAT      TO 102CN-EKHT-SUVAT                         
025936     MOVE FILB-EKH-DAAVIDAT   TO 102CN-EKHT-DAAVIDAT                      
025937     MOVE FILB-EKH-IDAVINR    TO 102CN-EKHT-IDAVINR                       
025938     MOVE FILB-EKH-IDLEVNR    TO 102CN-EKHT-IDLEVNR                       
025939     MOVE FILB-EKH-KDAVVTYP   TO 102CN-EKHT-KDAVVTYP                      
025940     MOVE FILB-EKH-KDRT       TO 102CN-EKHT-KDRT                          
025941     MOVE FILB-EKH-KVANTMOT   TO 102CN-EKHT-KVANTMOT                      
025942     MOVE FILB-EKH-KVAVIS     TO 102CN-EKHT-KVAVIS                        
025943     MOVE FILB-EKH-KDSORT     TO 102CN-EKHT-KDSORT                        
025944     MOVE FILB-EKH-KDTRADP    TO 102CN-EKHT-KDTRADP                       
025945     MOVE FILB-EKH-FLOVRLEV   TO 102CN-EKHT-FLOVRLEV                      
025946     IF FILB-EKH-IDORDNR5 NOT NUMERIC                                     
025947       MOVE ZERO              TO 102CN-EKHT-IDORDNR5                      
025948     ELSE                                                                 
025949       MOVE FILB-EKH-IDORDNR5 TO 102CN-EKHT-IDORDNR5                      
025950     END-IF                                                               
025951     MOVE FILB-EKH-IDUSER     TO 102CN-EKHT-IDUSER                        
025952     IF FILB-EKH-PRHEMTAG NOT NUMERIC                                     
025953       MOVE ZERO              TO 102CN-EKHT-PRHEMTAG                      
025954     ELSE                                                                 
025955       MOVE FILB-EKH-PRHEMTAG TO 102CN-EKHT-PRHEMTAG                      
025956     END-IF                                                               
025957     MOVE FILB-EKH-FLDCET     TO 102CN-EKHT-FLDCET                        
025958     MOVE FILB-EKH-IDKUNDRF   TO 102CN-EKHT-IDKUNDRF                      
025959     MOVE FILB-EKH-IDFAKT-EXP TO 102CN-EKHT-IDFAKT-EXP                    
025960                                                                          
025961     WRITE 102CN-POST   FROM 102CN-AREA                                   
025962                                                                          
025963     MOVE 'SAP'         TO POSTSUM-TRANSTYP                               
025964     MOVE 'W5104A'      TO POSTSUM-FDNAMN                                 
025965     MOVE 'W51039D5'    TO POSTSUM-DDNAMN2                                
025966     CALL POSTSUM USING POSTSUM-PARM                                      
025967     .                                                                    
025968     EJECT                                                                
025969                                                                          
025970 S19-SKRIV-W51042       SECTION.                                          
025971     MOVE FILB-FIL-IDPGM       TO SAPIN-EKHT-IDPGM                        
025972     MOVE FILB-FIL-TIREGDAT    TO SAPIN-EKHT-TIREGDAT                     
025973     MOVE FILB-FIL-TIKLOCK     TO SAPIN-EKHT-TIKLOCK                      
025974     MOVE FILB-FIL-IDSEKVNR    TO SAPIN-EKHT-IDSEKVNR                     
025975     MOVE FILB-FIL-CT-IDSYSTEM TO SAPIN-EKHT-CT-IDSYSTEM                  
025976     MOVE FILB-FIL-CT-IDPTYP  TO SAPIN-EKHT-CT-IDPTYP                     
025977     MOVE FILB-FIL-CT-IDVTYP  TO SAPIN-EKHT-CT-IDVTYP                     
025978     MOVE FILB-EKH-BEVAT      TO SAPIN-EKHT-BEVAT                         
025979     MOVE FILB-EKH-DAVERDAT   TO SAPIN-EKHT-DAVERDAT                      
025980     MOVE FILB-EKH-FLLSBOK    TO SAPIN-EKHT-FLLSBOK                       
025981     MOVE FILB-EKH-IDANALYS   TO SAPIN-EKHT-IDANALYS                      
025982     MOVE FILB-EKH-IDARTNR    TO SAPIN-EKHT-IDARTNR                       
025983     MOVE FILB-EKH-IDDC-SEND  TO SAPIN-EKHT-IDDC-SEND                     
025984     MOVE FILB-EKH-IDDC-REC   TO SAPIN-EKHT-IDDC-REC                      
025985     MOVE FILB-EKH-IDDISTR    TO SAPIN-EKHT-IDDISTR                       
025986     MOVE FILB-EKH-IDKONTO    TO SAPIN-EKHT-IDKONTO                       
025987     MOVE FILB-EKH-IDKST      TO SAPIN-EKHT-IDKST                         
025988     MOVE FILB-EKH-IDKUNDNR   TO SAPIN-EKHT-IDKUNDNR                      
025989     MOVE FILB-EKH-IDTRANS    TO SAPIN-EKHT-IDTRANS                       
025990     MOVE FILB-EKH-IDVERGL    TO SAPIN-EKHT-IDVERGL                       
025991     MOVE FILB-EKH-KDANMORS   TO SAPIN-EKHT-KDANMORS                      
025992     MOVE FILB-EKH-KDEKHHT    TO SAPIN-EKHT-KDEKHHT                       
025993     MOVE FILB-EKH-KDEKSHT    TO SAPIN-EKHT-KDEKSHT                       
025994     MOVE FILB-EKH-KDEKNIVA   TO SAPIN-EKHT-KDEKNIVA                      
025995     MOVE FILB-EKH-KDFRAKT    TO SAPIN-EKHT-KDFRAKT                       
025996     MOVE FILB-EKH-KDPRODSL   TO SAPIN-EKHT-KDPRODSL                      
025997     MOVE FILB-EKH-KDPSLLOC   TO SAPIN-EKHT-KDPSLLOC                      
025998     MOVE FILB-EKH-KDVALISO   TO SAPIN-EKHT-KDVALISO                      
025999     MOVE FILB-EKH-KVANTAL    TO SAPIN-EKHT-KVANTAL                       
026000     MOVE FILB-EKH-PRARTNTO   TO SAPIN-EKHT-PRARTNTO                      
026001     MOVE FILB-EKH-PRARTSJK   TO SAPIN-EKHT-PRARTSJK                      
026002     MOVE FILB-EKH-PRARTSTD   TO SAPIN-EKHT-PRARTSTD                      
026003     MOVE FILB-EKH-PRDIRLON   TO SAPIN-EKHT-PRDIRLON                      
026004     MOVE FILB-EKH-PRDMTRL    TO SAPIN-EKHT-PRDMTRL                       
026005     MOVE FILB-EKH-PRINK      TO SAPIN-EKHT-PRINK                         
026006     MOVE FILB-EKH-PRKURS     TO SAPIN-EKHT-PRKURS                        
026007     MOVE FILB-EKH-PRLANDCO   TO SAPIN-EKHT-PRLANDCO                      
026008     MOVE FILB-EKH-PROVRPAL   TO SAPIN-EKHT-PROVRPAL                      
026009     MOVE FILB-EKH-SUBEL      TO SAPIN-EKHT-SUBEL                         
026010     MOVE FILB-EKH-SUVAT      TO SAPIN-EKHT-SUVAT                         
026011     MOVE FILB-EKH-DAAVIDAT   TO SAPIN-EKHT-DAAVIDAT                      
026012     MOVE FILB-EKH-IDAVINR    TO SAPIN-EKHT-IDAVINR                       
026013     MOVE FILB-EKH-IDLEVNR    TO SAPIN-EKHT-IDLEVNR                       
026014     MOVE FILB-EKH-KDAVVTYP   TO SAPIN-EKHT-KDAVVTYP                      
026015     MOVE FILB-EKH-KDRT       TO SAPIN-EKHT-KDRT                          
026016     MOVE FILB-EKH-KVANTMOT   TO SAPIN-EKHT-KVANTMOT                      
026017     MOVE FILB-EKH-KVAVIS     TO SAPIN-EKHT-KVAVIS                        
026018     MOVE FILB-EKH-KDSORT     TO SAPIN-EKHT-KDSORT                        
026019     MOVE FILB-EKH-KDTRADP    TO SAPIN-EKHT-KDTRADP                       
026020     MOVE FILB-EKH-FLOVRLEV   TO SAPIN-EKHT-FLOVRLEV                      
026021     IF FILB-EKH-IDORDNR5 NOT NUMERIC                                     
026022       MOVE ZERO              TO SAPIN-EKHT-IDORDNR5                      
026023     ELSE                                                                 
026024       MOVE FILB-EKH-IDORDNR5 TO SAPIN-EKHT-IDORDNR5                      
026025     END-IF                                                               
026026     MOVE FILB-EKH-IDUSER     TO SAPIN-EKHT-IDUSER                        
026027     IF FILB-EKH-PRHEMTAG NOT NUMERIC                                     
026028       MOVE ZERO              TO SAPIN-EKHT-PRHEMTAG                      
026029     ELSE                                                                 
026030       MOVE FILB-EKH-PRHEMTAG TO SAPIN-EKHT-PRHEMTAG                      
026031     END-IF                                                               
026032     MOVE FILB-EKH-FLDCET     TO SAPIN-EKHT-FLDCET                        
026033     MOVE FILB-EKH-IDKUNDRF   TO SAPIN-EKHT-IDKUNDRF                      
026034     MOVE FILB-EKH-IDFAKT-EXP TO SAPIN-EKHT-IDFAKT-EXP                    
026035                                                                          
026036     WRITE SAPIN-POST   FROM SAPIN-AREA                                   
026037                                                                          
026038     MOVE 'SAP'         TO POSTSUM-TRANSTYP                               
026039     MOVE 'W51042'      TO POSTSUM-FDNAMN                                 
026040     MOVE 'W51039D6'    TO POSTSUM-DDNAMN2                                
026041     CALL POSTSUM USING POSTSUM-PARM                                      
026042     .                                                                    
026043     EJECT                                                                
026044                                                                          
026045 S19-SKRIV-W5104B       SECTION.                                          
026046     MOVE FILB-FIL-IDPGM       TO 102IN-EKHT-IDPGM                        
026047     MOVE FILB-FIL-TIREGDAT    TO 102IN-EKHT-TIREGDAT                     
026048     MOVE FILB-FIL-TIKLOCK     TO 102IN-EKHT-TIKLOCK                      
026049     MOVE FILB-FIL-IDSEKVNR    TO 102IN-EKHT-IDSEKVNR                     
026050     MOVE FILB-FIL-CT-IDSYSTEM TO 102IN-EKHT-CT-IDSYSTEM                  
026051     MOVE FILB-FIL-CT-IDPTYP  TO 102IN-EKHT-CT-IDPTYP                     
026052     MOVE FILB-FIL-CT-IDVTYP  TO 102IN-EKHT-CT-IDVTYP                     
026053     MOVE FILB-EKH-BEVAT      TO 102IN-EKHT-BEVAT                         
026054     MOVE FILB-EKH-DAVERDAT   TO 102IN-EKHT-DAVERDAT                      
026055     MOVE FILB-EKH-FLLSBOK    TO 102IN-EKHT-FLLSBOK                       
026056     MOVE FILB-EKH-IDANALYS   TO 102IN-EKHT-IDANALYS                      
026057     MOVE FILB-EKH-IDARTNR    TO 102IN-EKHT-IDARTNR                       
026058     MOVE FILB-EKH-IDDC-SEND  TO 102IN-EKHT-IDDC-SEND                     
026059     MOVE FILB-EKH-IDDC-REC   TO 102IN-EKHT-IDDC-REC                      
026060     MOVE FILB-EKH-IDDISTR    TO 102IN-EKHT-IDDISTR                       
026061     MOVE FILB-EKH-IDKONTO    TO 102IN-EKHT-IDKONTO                       
026062     MOVE FILB-EKH-IDKST      TO 102IN-EKHT-IDKST                         
026063     MOVE FILB-EKH-IDKUNDNR   TO 102IN-EKHT-IDKUNDNR                      
026064     MOVE FILB-EKH-IDTRANS    TO 102IN-EKHT-IDTRANS                       
026065     MOVE FILB-EKH-IDVERGL    TO 102IN-EKHT-IDVERGL                       
026066     MOVE FILB-EKH-KDANMORS   TO 102IN-EKHT-KDANMORS                      
026067     MOVE FILB-EKH-KDEKHHT    TO 102IN-EKHT-KDEKHHT                       
026068     MOVE FILB-EKH-KDEKSHT    TO 102IN-EKHT-KDEKSHT                       
026069     MOVE FILB-EKH-KDEKNIVA   TO 102IN-EKHT-KDEKNIVA                      
026070     MOVE FILB-EKH-KDFRAKT    TO 102IN-EKHT-KDFRAKT                       
026071     MOVE FILB-EKH-KDPRODSL   TO 102IN-EKHT-KDPRODSL                      
026072     MOVE FILB-EKH-KDPSLLOC   TO 102IN-EKHT-KDPSLLOC                      
026073     MOVE FILB-EKH-KDVALISO   TO 102IN-EKHT-KDVALISO                      
026074     MOVE FILB-EKH-KVANTAL    TO 102IN-EKHT-KVANTAL                       
026075     MOVE FILB-EKH-PRARTNTO   TO 102IN-EKHT-PRARTNTO                      
026076     MOVE FILB-EKH-PRARTSJK   TO 102IN-EKHT-PRARTSJK                      
026077     MOVE FILB-EKH-PRARTSTD   TO 102IN-EKHT-PRARTSTD                      
026078     MOVE FILB-EKH-PRDIRLON   TO 102IN-EKHT-PRDIRLON                      
026079     MOVE FILB-EKH-PRDMTRL    TO 102IN-EKHT-PRDMTRL                       
026080     MOVE FILB-EKH-PRINK      TO 102IN-EKHT-PRINK                         
026081     MOVE FILB-EKH-PRKURS     TO 102IN-EKHT-PRKURS                        
026082     MOVE FILB-EKH-PRLANDCO   TO 102IN-EKHT-PRLANDCO                      
026083     MOVE FILB-EKH-PROVRPAL   TO 102IN-EKHT-PROVRPAL                      
026084     MOVE FILB-EKH-SUBEL      TO 102IN-EKHT-SUBEL                         
026085     MOVE FILB-EKH-SUVAT      TO 102IN-EKHT-SUVAT                         
026086     MOVE FILB-EKH-DAAVIDAT   TO 102IN-EKHT-DAAVIDAT                      
026087     MOVE FILB-EKH-IDAVINR    TO 102IN-EKHT-IDAVINR                       
026088     MOVE FILB-EKH-IDLEVNR    TO 102IN-EKHT-IDLEVNR                       
026089     MOVE FILB-EKH-KDAVVTYP   TO 102IN-EKHT-KDAVVTYP                      
026090     MOVE FILB-EKH-KDRT       TO 102IN-EKHT-KDRT                          
026091     MOVE FILB-EKH-KVANTMOT   TO 102IN-EKHT-KVANTMOT                      
026092     MOVE FILB-EKH-KVAVIS     TO 102IN-EKHT-KVAVIS                        
026093     MOVE FILB-EKH-KDSORT     TO 102IN-EKHT-KDSORT                        
026094     MOVE FILB-EKH-KDTRADP    TO 102IN-EKHT-KDTRADP                       
026095     MOVE FILB-EKH-FLOVRLEV   TO 102IN-EKHT-FLOVRLEV                      
026096     IF FILB-EKH-IDORDNR5 NOT NUMERIC                                     
026097       MOVE ZERO              TO 102IN-EKHT-IDORDNR5                      
026098     ELSE                                                                 
026099       MOVE FILB-EKH-IDORDNR5 TO 102IN-EKHT-IDORDNR5                      
026100     END-IF                                                               
026101     MOVE FILB-EKH-IDUSER     TO 102IN-EKHT-IDUSER                        
026102     IF FILB-EKH-PRHEMTAG NOT NUMERIC                                     
026103       MOVE ZERO              TO 102IN-EKHT-PRHEMTAG                      
026104     ELSE                                                                 
026105       MOVE FILB-EKH-PRHEMTAG TO 102IN-EKHT-PRHEMTAG                      
026106     END-IF                                                               
026107     MOVE FILB-EKH-FLDCET     TO 102IN-EKHT-FLDCET                        
026108     MOVE FILB-EKH-IDKUNDRF   TO 102IN-EKHT-IDKUNDRF                      
026109     MOVE FILB-EKH-IDFAKT-EXP TO 102IN-EKHT-IDFAKT-EXP                    
026110                                                                          
026111     WRITE 102IN-POST   FROM 102IN-AREA                                   
026112                                                                          
026113     MOVE 'SAP'         TO POSTSUM-TRANSTYP                               
026114     MOVE 'W5104B'      TO POSTSUM-FDNAMN                                 
026115     MOVE 'W51039D7'    TO POSTSUM-DDNAMN2                                
026116     CALL POSTSUM USING POSTSUM-PARM                                      
026117     .                                                                    
026118     EJECT                                                                
026119 S19-SKRIV-W51043       SECTION.                                          
026120     MOVE FILB-FIL-IDPGM       TO SAPUS-EKHT-IDPGM                        
026121     MOVE FILB-FIL-TIREGDAT    TO SAPUS-EKHT-TIREGDAT                     
026122     MOVE FILB-FIL-TIKLOCK     TO SAPUS-EKHT-TIKLOCK                      
026123     MOVE FILB-FIL-IDSEKVNR    TO SAPUS-EKHT-IDSEKVNR                     
026124     MOVE FILB-FIL-CT-IDSYSTEM TO SAPUS-EKHT-CT-IDSYSTEM                  
026125     MOVE FILB-FIL-CT-IDPTYP  TO SAPUS-EKHT-CT-IDPTYP                     
026126     MOVE FILB-FIL-CT-IDVTYP  TO SAPUS-EKHT-CT-IDVTYP                     
026127     MOVE FILB-EKH-BEVAT      TO SAPUS-EKHT-BEVAT                         
026128     MOVE FILB-EKH-DAVERDAT   TO SAPUS-EKHT-DAVERDAT                      
026129     MOVE FILB-EKH-FLLSBOK    TO SAPUS-EKHT-FLLSBOK                       
026130     MOVE FILB-EKH-IDANALYS   TO SAPUS-EKHT-IDANALYS                      
026131     MOVE FILB-EKH-IDARTNR    TO SAPUS-EKHT-IDARTNR                       
026132     MOVE FILB-EKH-IDDC-SEND  TO SAPUS-EKHT-IDDC-SEND                     
026133     MOVE FILB-EKH-IDDC-REC   TO SAPUS-EKHT-IDDC-REC                      
026134     MOVE FILB-EKH-IDDISTR    TO SAPUS-EKHT-IDDISTR                       
026135     MOVE FILB-EKH-IDKONTO    TO SAPUS-EKHT-IDKONTO                       
026136     MOVE FILB-EKH-IDKST      TO SAPUS-EKHT-IDKST                         
026137     MOVE FILB-EKH-IDKUNDNR   TO SAPUS-EKHT-IDKUNDNR                      
026138     MOVE FILB-EKH-IDTRANS    TO SAPUS-EKHT-IDTRANS                       
026139     MOVE FILB-EKH-IDVERGL    TO SAPUS-EKHT-IDVERGL                       
026140     MOVE FILB-EKH-KDANMORS   TO SAPUS-EKHT-KDANMORS                      
026141     MOVE FILB-EKH-KDEKHHT    TO SAPUS-EKHT-KDEKHHT                       
026142     MOVE FILB-EKH-KDEKSHT    TO SAPUS-EKHT-KDEKSHT                       
026143     MOVE FILB-EKH-KDEKNIVA   TO SAPUS-EKHT-KDEKNIVA                      
026144     MOVE FILB-EKH-KDFRAKT    TO SAPUS-EKHT-KDFRAKT                       
026145     MOVE FILB-EKH-KDPRODSL   TO SAPUS-EKHT-KDPRODSL                      
026146     MOVE FILB-EKH-KDPSLLOC   TO SAPUS-EKHT-KDPSLLOC                      
026147     MOVE FILB-EKH-KDVALISO   TO SAPUS-EKHT-KDVALISO                      
026148     MOVE FILB-EKH-KVANTAL    TO SAPUS-EKHT-KVANTAL                       
026149     MOVE FILB-EKH-PRARTNTO   TO SAPUS-EKHT-PRARTNTO                      
026150     MOVE FILB-EKH-PRARTSJK   TO SAPUS-EKHT-PRARTSJK                      
026151     MOVE FILB-EKH-PRARTSTD   TO SAPUS-EKHT-PRARTSTD                      
026152     MOVE FILB-EKH-PRDIRLON   TO SAPUS-EKHT-PRDIRLON                      
026153     MOVE FILB-EKH-PRDMTRL    TO SAPUS-EKHT-PRDMTRL                       
026154     MOVE FILB-EKH-PRINK      TO SAPUS-EKHT-PRINK                         
026155     MOVE FILB-EKH-PRKURS     TO SAPUS-EKHT-PRKURS                        
026156     MOVE FILB-EKH-PRLANDCO   TO SAPUS-EKHT-PRLANDCO                      
026157     MOVE FILB-EKH-PROVRPAL   TO SAPUS-EKHT-PROVRPAL                      
026158     MOVE FILB-EKH-SUBEL      TO SAPUS-EKHT-SUBEL                         
026159     MOVE FILB-EKH-SUVAT      TO SAPUS-EKHT-SUVAT                         
026160     MOVE FILB-EKH-DAAVIDAT   TO SAPUS-EKHT-DAAVIDAT                      
026161     MOVE FILB-EKH-IDAVINR    TO SAPUS-EKHT-IDAVINR                       
026162     MOVE FILB-EKH-IDLEVNR    TO SAPUS-EKHT-IDLEVNR                       
026163     MOVE FILB-EKH-KDAVVTYP   TO SAPUS-EKHT-KDAVVTYP                      
026164     MOVE FILB-EKH-KDRT       TO SAPUS-EKHT-KDRT                          
026165     MOVE FILB-EKH-KVANTMOT   TO SAPUS-EKHT-KVANTMOT                      
026166     MOVE FILB-EKH-KVAVIS     TO SAPUS-EKHT-KVAVIS                        
026167     MOVE FILB-EKH-KDSORT     TO SAPUS-EKHT-KDSORT                        
026168     MOVE FILB-EKH-KDTRADP    TO SAPUS-EKHT-KDTRADP                       
026169     MOVE FILB-EKH-FLOVRLEV   TO SAPUS-EKHT-FLOVRLEV                      
026170     IF FILB-EKH-IDORDNR5 NOT NUMERIC                                     
026171       MOVE ZERO              TO SAPUS-EKHT-IDORDNR5                      
026172     ELSE                                                                 
026173       MOVE FILB-EKH-IDORDNR5 TO SAPUS-EKHT-IDORDNR5                      
026174     END-IF                                                               
026175     MOVE FILB-EKH-IDUSER     TO SAPUS-EKHT-IDUSER                        
026176     IF FILB-EKH-PRHEMTAG NOT NUMERIC                                     
026177       MOVE ZERO              TO SAPUS-EKHT-PRHEMTAG                      
026178     ELSE                                                                 
026179       MOVE FILB-EKH-PRHEMTAG TO SAPUS-EKHT-PRHEMTAG                      
026180     END-IF                                                               
026181     MOVE FILB-EKH-FLDCET     TO SAPUS-EKHT-FLDCET                        
026182     MOVE FILB-EKH-IDKUNDRF   TO SAPUS-EKHT-IDKUNDRF                      
026183     MOVE FILB-EKH-IDFAKT-EXP TO SAPUS-EKHT-IDFAKT-EXP                    
026184                                                                          
026185     WRITE SAPUS-POST   FROM SAPUS-AREA                                   
026186                                                                          
026187     MOVE 'SAP'         TO POSTSUM-TRANSTYP                               
026188     MOVE 'W51043'      TO POSTSUM-FDNAMN                                 
026189     MOVE 'W51039D8'    TO POSTSUM-DDNAMN2                                
026190     CALL POSTSUM USING POSTSUM-PARM                                      
026191     .                                                                    
026192     EJECT                                                                
026193                                                                          
026194 S19-SKRIV-W5104C       SECTION.                                          
026195     MOVE FILB-FIL-IDPGM       TO 102US-EKHT-IDPGM                        
026196     MOVE FILB-FIL-TIREGDAT    TO 102US-EKHT-TIREGDAT                     
026197     MOVE FILB-FIL-TIKLOCK     TO 102US-EKHT-TIKLOCK                      
026198     MOVE FILB-FIL-IDSEKVNR    TO 102US-EKHT-IDSEKVNR                     
026199     MOVE FILB-FIL-CT-IDSYSTEM TO 102US-EKHT-CT-IDSYSTEM                  
026200     MOVE FILB-FIL-CT-IDPTYP  TO 102US-EKHT-CT-IDPTYP                     
026201     MOVE FILB-FIL-CT-IDVTYP  TO 102US-EKHT-CT-IDVTYP                     
026202     MOVE FILB-EKH-BEVAT      TO 102US-EKHT-BEVAT                         
026203     MOVE FILB-EKH-DAVERDAT   TO 102US-EKHT-DAVERDAT                      
026204     MOVE FILB-EKH-FLLSBOK    TO 102US-EKHT-FLLSBOK                       
026205     MOVE FILB-EKH-IDANALYS   TO 102US-EKHT-IDANALYS                      
026206     MOVE FILB-EKH-IDARTNR    TO 102US-EKHT-IDARTNR                       
026207     MOVE FILB-EKH-IDDC-SEND  TO 102US-EKHT-IDDC-SEND                     
026208     MOVE FILB-EKH-IDDC-REC   TO 102US-EKHT-IDDC-REC                      
026209     MOVE FILB-EKH-IDDISTR    TO 102US-EKHT-IDDISTR                       
026210     MOVE FILB-EKH-IDKONTO    TO 102US-EKHT-IDKONTO                       
026211     MOVE FILB-EKH-IDKST      TO 102US-EKHT-IDKST                         
026212     MOVE FILB-EKH-IDKUNDNR   TO 102US-EKHT-IDKUNDNR                      
026213     MOVE FILB-EKH-IDTRANS    TO 102US-EKHT-IDTRANS                       
026214     MOVE FILB-EKH-IDVERGL    TO 102US-EKHT-IDVERGL                       
026215     MOVE FILB-EKH-KDANMORS   TO 102US-EKHT-KDANMORS                      
026216     MOVE FILB-EKH-KDEKHHT    TO 102US-EKHT-KDEKHHT                       
026217     MOVE FILB-EKH-KDEKSHT    TO 102US-EKHT-KDEKSHT                       
026218     MOVE FILB-EKH-KDEKNIVA   TO 102US-EKHT-KDEKNIVA                      
026219     MOVE FILB-EKH-KDFRAKT    TO 102US-EKHT-KDFRAKT                       
026220     MOVE FILB-EKH-KDPRODSL   TO 102US-EKHT-KDPRODSL                      
026221     MOVE FILB-EKH-KDPSLLOC   TO 102US-EKHT-KDPSLLOC                      
026222     MOVE FILB-EKH-KDVALISO   TO 102US-EKHT-KDVALISO                      
026223     MOVE FILB-EKH-KVANTAL    TO 102US-EKHT-KVANTAL                       
026224     MOVE FILB-EKH-PRARTNTO   TO 102US-EKHT-PRARTNTO                      
026225     MOVE FILB-EKH-PRARTSJK   TO 102US-EKHT-PRARTSJK                      
026226     MOVE FILB-EKH-PRARTSTD   TO 102US-EKHT-PRARTSTD                      
026227     MOVE FILB-EKH-PRDIRLON   TO 102US-EKHT-PRDIRLON                      
026228     MOVE FILB-EKH-PRDMTRL    TO 102US-EKHT-PRDMTRL                       
026229     MOVE FILB-EKH-PRINK      TO 102US-EKHT-PRINK                         
026230     MOVE FILB-EKH-PRKURS     TO 102US-EKHT-PRKURS                        
026231     MOVE FILB-EKH-PRLANDCO   TO 102US-EKHT-PRLANDCO                      
026232     MOVE FILB-EKH-PROVRPAL   TO 102US-EKHT-PROVRPAL                      
026233     MOVE FILB-EKH-SUBEL      TO 102US-EKHT-SUBEL                         
026234     MOVE FILB-EKH-SUVAT      TO 102US-EKHT-SUVAT                         
026235     MOVE FILB-EKH-DAAVIDAT   TO 102US-EKHT-DAAVIDAT                      
026236     MOVE FILB-EKH-IDAVINR    TO 102US-EKHT-IDAVINR                       
026237     MOVE FILB-EKH-IDLEVNR    TO 102US-EKHT-IDLEVNR                       
026238     MOVE FILB-EKH-KDAVVTYP   TO 102US-EKHT-KDAVVTYP                      
026239     MOVE FILB-EKH-KDRT       TO 102US-EKHT-KDRT                          
026240     MOVE FILB-EKH-KVANTMOT   TO 102US-EKHT-KVANTMOT                      
026241     MOVE FILB-EKH-KVAVIS     TO 102US-EKHT-KVAVIS                        
026242     MOVE FILB-EKH-KDSORT     TO 102US-EKHT-KDSORT                        
026243     MOVE FILB-EKH-KDTRADP    TO 102US-EKHT-KDTRADP                       
026244     MOVE FILB-EKH-FLOVRLEV   TO 102US-EKHT-FLOVRLEV                      
026245     IF FILB-EKH-IDORDNR5 NOT NUMERIC                                     
026246       MOVE ZERO              TO 102US-EKHT-IDORDNR5                      
026247     ELSE                                                                 
026248       MOVE FILB-EKH-IDORDNR5 TO 102US-EKHT-IDORDNR5                      
026249     END-IF                                                               
026250     MOVE FILB-EKH-IDUSER     TO 102US-EKHT-IDUSER                        
026251     IF FILB-EKH-PRHEMTAG NOT NUMERIC                                     
026252       MOVE ZERO              TO 102US-EKHT-PRHEMTAG                      
026253     ELSE                                                                 
026254       MOVE FILB-EKH-PRHEMTAG TO 102US-EKHT-PRHEMTAG                      
026255     END-IF                                                               
026256     MOVE FILB-EKH-FLDCET     TO 102US-EKHT-FLDCET                        
026257     MOVE FILB-EKH-IDKUNDRF   TO 102US-EKHT-IDKUNDRF                      
026258     MOVE FILB-EKH-IDFAKT-EXP TO 102US-EKHT-IDFAKT-EXP                    
026259                                                                          
026260     WRITE 102US-POST   FROM 102US-AREA                                   
026261                                                                          
026262     MOVE 'SAP'         TO POSTSUM-TRANSTYP                               
026263     MOVE 'W5104C'      TO POSTSUM-FDNAMN                                 
026264     MOVE 'W51039D9'    TO POSTSUM-DDNAMN2                                
026265     CALL POSTSUM USING POSTSUM-PARM                                      
026266     .                                                                    
026267     EJECT                                                                
026268 S19-SKRIV-W51047       SECTION.                                          
026269     MOVE FILB-FIL-IDPGM       TO SAPXX-EKHT-IDPGM                        
026270     MOVE FILB-FIL-TIREGDAT    TO SAPXX-EKHT-TIREGDAT                     
026271     MOVE FILB-FIL-TIKLOCK     TO SAPXX-EKHT-TIKLOCK                      
026272     MOVE FILB-FIL-IDSEKVNR    TO SAPXX-EKHT-IDSEKVNR                     
026273     MOVE FILB-FIL-CT-IDSYSTEM TO SAPXX-EKHT-CT-IDSYSTEM                  
026274     MOVE FILB-FIL-CT-IDPTYP  TO SAPXX-EKHT-CT-IDPTYP                     
026275     MOVE FILB-FIL-CT-IDVTYP  TO SAPXX-EKHT-CT-IDVTYP                     
026276     MOVE FILB-EKH-BEVAT      TO SAPXX-EKHT-BEVAT                         
026277     MOVE FILB-EKH-DAVERDAT   TO SAPXX-EKHT-DAVERDAT                      
026278     MOVE FILB-EKH-FLLSBOK    TO SAPXX-EKHT-FLLSBOK                       
026279     MOVE FILB-EKH-IDANALYS   TO SAPXX-EKHT-IDANALYS                      
026280     MOVE FILB-EKH-IDARTNR    TO SAPXX-EKHT-IDARTNR                       
026281     MOVE FILB-EKH-IDDC-SEND  TO SAPXX-EKHT-IDDC-SEND                     
026282     MOVE FILB-EKH-IDDC-REC   TO SAPXX-EKHT-IDDC-REC                      
026283     MOVE FILB-EKH-IDDISTR    TO SAPXX-EKHT-IDDISTR                       
026284     MOVE FILB-EKH-IDKONTO    TO SAPXX-EKHT-IDKONTO                       
026285     MOVE FILB-EKH-IDKST      TO SAPXX-EKHT-IDKST                         
026286     MOVE FILB-EKH-IDKUNDNR   TO SAPXX-EKHT-IDKUNDNR                      
026287     MOVE FILB-EKH-IDTRANS    TO SAPXX-EKHT-IDTRANS                       
026288     MOVE FILB-EKH-IDVERGL    TO SAPXX-EKHT-IDVERGL                       
026289     MOVE FILB-EKH-KDANMORS   TO SAPXX-EKHT-KDANMORS                      
026290     MOVE FILB-EKH-KDEKHHT    TO SAPXX-EKHT-KDEKHHT                       
026291     MOVE FILB-EKH-KDEKSHT    TO SAPXX-EKHT-KDEKSHT                       
026292     MOVE FILB-EKH-KDEKNIVA   TO SAPXX-EKHT-KDEKNIVA                      
026293     MOVE FILB-EKH-KDFRAKT    TO SAPXX-EKHT-KDFRAKT                       
026294     MOVE FILB-EKH-KDPRODSL   TO SAPXX-EKHT-KDPRODSL                      
026295     MOVE FILB-EKH-KDPSLLOC   TO SAPXX-EKHT-KDPSLLOC                      
026296     MOVE FILB-EKH-KDVALISO   TO SAPXX-EKHT-KDVALISO                      
026297     MOVE FILB-EKH-KVANTAL    TO SAPXX-EKHT-KVANTAL                       
026298     MOVE FILB-EKH-PRARTNTO   TO SAPXX-EKHT-PRARTNTO                      
026299     MOVE FILB-EKH-PRARTSJK   TO SAPXX-EKHT-PRARTSJK                      
026300     MOVE FILB-EKH-PRARTSTD   TO SAPXX-EKHT-PRARTSTD                      
026301     MOVE FILB-EKH-PRDIRLON   TO SAPXX-EKHT-PRDIRLON                      
026302     MOVE FILB-EKH-PRDMTRL    TO SAPXX-EKHT-PRDMTRL                       
026303     MOVE FILB-EKH-PRINK      TO SAPXX-EKHT-PRINK                         
026304     MOVE FILB-EKH-PRKURS     TO SAPXX-EKHT-PRKURS                        
026305     MOVE FILB-EKH-PRLANDCO   TO SAPXX-EKHT-PRLANDCO                      
026306     MOVE FILB-EKH-PROVRPAL   TO SAPXX-EKHT-PROVRPAL                      
026307     MOVE FILB-EKH-SUBEL      TO SAPXX-EKHT-SUBEL                         
026308     MOVE FILB-EKH-SUVAT      TO SAPXX-EKHT-SUVAT                         
026309     MOVE FILB-EKH-DAAVIDAT   TO SAPXX-EKHT-DAAVIDAT                      
026310     MOVE FILB-EKH-IDAVINR    TO SAPXX-EKHT-IDAVINR                       
026311     MOVE FILB-EKH-IDLEVNR    TO SAPXX-EKHT-IDLEVNR                       
026312     MOVE FILB-EKH-KDAVVTYP   TO SAPXX-EKHT-KDAVVTYP                      
026313     MOVE FILB-EKH-KDRT       TO SAPXX-EKHT-KDRT                          
026314     MOVE FILB-EKH-KVANTMOT   TO SAPXX-EKHT-KVANTMOT                      
026315     MOVE FILB-EKH-KVAVIS     TO SAPXX-EKHT-KVAVIS                        
026316     MOVE FILB-EKH-KDSORT     TO SAPXX-EKHT-KDSORT                        
026317     MOVE FILB-EKH-KDTRADP    TO SAPXX-EKHT-KDTRADP                       
026318     MOVE FILB-EKH-FLOVRLEV   TO SAPXX-EKHT-FLOVRLEV                      
026319     IF FILB-EKH-IDORDNR5 NOT NUMERIC                                     
026320       MOVE ZERO              TO SAPXX-EKHT-IDORDNR5                      
026321     ELSE                                                                 
026322       MOVE FILB-EKH-IDORDNR5 TO SAPXX-EKHT-IDORDNR5                      
026323     END-IF                                                               
026324     MOVE FILB-EKH-IDUSER     TO SAPXX-EKHT-IDUSER                        
026325     IF FILB-EKH-PRHEMTAG NOT NUMERIC                                     
026326       MOVE ZERO              TO SAPXX-EKHT-PRHEMTAG                      
026327     ELSE                                                                 
026328       MOVE FILB-EKH-PRHEMTAG TO SAPXX-EKHT-PRHEMTAG                      
026329     END-IF                                                               
026330     MOVE FILB-EKH-FLDCET     TO SAPXX-EKHT-FLDCET                        
026331     MOVE FILB-EKH-IDKUNDRF   TO SAPXX-EKHT-IDKUNDRF                      
026332     MOVE FILB-EKH-IDFAKT-EXP TO SAPXX-EKHT-IDFAKT-EXP                    
026333     MOVE FILB-EKH-CMD        TO SAPXX-EKHT-CMD                           
026334                                                                          
026335     WRITE SAPXX-POST   FROM SAPXX-AREA                                   
026336                                                                          
026337     MOVE 'SAP'         TO POSTSUM-TRANSTYP                               
026338     MOVE 'W51047'      TO POSTSUM-FDNAMN                                 
026339     MOVE 'W51039DA'    TO POSTSUM-DDNAMN2                                
026340     CALL POSTSUM USING POSTSUM-PARM                                      
026341     .                                                                    
026342     EJECT                                                                
026343 S19-SKRIV-W5104D       SECTION.                                          
026344     MOVE FILB-FIL-IDPGM       TO 102XX-EKHT-IDPGM                        
026345     MOVE FILB-FIL-TIREGDAT    TO 102XX-EKHT-TIREGDAT                     
026346     MOVE FILB-FIL-TIKLOCK     TO 102XX-EKHT-TIKLOCK                      
026347     MOVE FILB-FIL-IDSEKVNR    TO 102XX-EKHT-IDSEKVNR                     
026348     MOVE FILB-FIL-CT-IDSYSTEM TO 102XX-EKHT-CT-IDSYSTEM                  
026349     MOVE FILB-FIL-CT-IDPTYP  TO 102XX-EKHT-CT-IDPTYP                     
026350     MOVE FILB-FIL-CT-IDVTYP  TO 102XX-EKHT-CT-IDVTYP                     
026351     MOVE FILB-EKH-BEVAT      TO 102XX-EKHT-BEVAT                         
026352     MOVE FILB-EKH-DAVERDAT   TO 102XX-EKHT-DAVERDAT                      
026353     MOVE FILB-EKH-FLLSBOK    TO 102XX-EKHT-FLLSBOK                       
026354     MOVE FILB-EKH-IDANALYS   TO 102XX-EKHT-IDANALYS                      
026355     MOVE FILB-EKH-IDARTNR    TO 102XX-EKHT-IDARTNR                       
026356     MOVE FILB-EKH-IDDC-SEND  TO 102XX-EKHT-IDDC-SEND                     
026357     MOVE FILB-EKH-IDDC-REC   TO 102XX-EKHT-IDDC-REC                      
026358     MOVE FILB-EKH-IDDISTR    TO 102XX-EKHT-IDDISTR                       
026359     MOVE FILB-EKH-IDKONTO    TO 102XX-EKHT-IDKONTO                       
026360     MOVE FILB-EKH-IDKST      TO 102XX-EKHT-IDKST                         
026361     MOVE FILB-EKH-IDKUNDNR   TO 102XX-EKHT-IDKUNDNR                      
026362     MOVE FILB-EKH-IDTRANS    TO 102XX-EKHT-IDTRANS                       
026363     MOVE FILB-EKH-IDVERGL    TO 102XX-EKHT-IDVERGL                       
026364     MOVE FILB-EKH-KDANMORS   TO 102XX-EKHT-KDANMORS                      
026365     MOVE FILB-EKH-KDEKHHT    TO 102XX-EKHT-KDEKHHT                       
026366     MOVE FILB-EKH-KDEKSHT    TO 102XX-EKHT-KDEKSHT                       
026367     MOVE FILB-EKH-KDEKNIVA   TO 102XX-EKHT-KDEKNIVA                      
026368     MOVE FILB-EKH-KDFRAKT    TO 102XX-EKHT-KDFRAKT                       
026369     MOVE FILB-EKH-KDPRODSL   TO 102XX-EKHT-KDPRODSL                      
026370     MOVE FILB-EKH-KDPSLLOC   TO 102XX-EKHT-KDPSLLOC                      
026371     MOVE FILB-EKH-KDVALISO   TO 102XX-EKHT-KDVALISO                      
026372     MOVE FILB-EKH-KVANTAL    TO 102XX-EKHT-KVANTAL                       
026373     MOVE FILB-EKH-PRARTNTO   TO 102XX-EKHT-PRARTNTO                      
026374     MOVE FILB-EKH-PRARTSJK   TO 102XX-EKHT-PRARTSJK                      
026375     MOVE FILB-EKH-PRARTSTD   TO 102XX-EKHT-PRARTSTD                      
026376     MOVE FILB-EKH-PRDIRLON   TO 102XX-EKHT-PRDIRLON                      
026377     MOVE FILB-EKH-PRDMTRL    TO 102XX-EKHT-PRDMTRL                       
026378     MOVE FILB-EKH-PRINK      TO 102XX-EKHT-PRINK                         
026379     MOVE FILB-EKH-PRKURS     TO 102XX-EKHT-PRKURS                        
026380     MOVE FILB-EKH-PRLANDCO   TO 102XX-EKHT-PRLANDCO                      
026381     MOVE FILB-EKH-PROVRPAL   TO 102XX-EKHT-PROVRPAL                      
026382     MOVE FILB-EKH-SUBEL      TO 102XX-EKHT-SUBEL                         
026383     MOVE FILB-EKH-SUVAT      TO 102XX-EKHT-SUVAT                         
026384     MOVE FILB-EKH-DAAVIDAT   TO 102XX-EKHT-DAAVIDAT                      
026385     MOVE FILB-EKH-IDAVINR    TO 102XX-EKHT-IDAVINR                       
026386     MOVE FILB-EKH-IDLEVNR    TO 102XX-EKHT-IDLEVNR                       
026387     MOVE FILB-EKH-KDAVVTYP   TO 102XX-EKHT-KDAVVTYP                      
026388     MOVE FILB-EKH-KDRT       TO 102XX-EKHT-KDRT                          
026389     MOVE FILB-EKH-KVANTMOT   TO 102XX-EKHT-KVANTMOT                      
026390     MOVE FILB-EKH-KVAVIS     TO 102XX-EKHT-KVAVIS                        
026391     MOVE FILB-EKH-KDSORT     TO 102XX-EKHT-KDSORT                        
026392     MOVE FILB-EKH-KDTRADP    TO 102XX-EKHT-KDTRADP                       
026393     MOVE FILB-EKH-FLOVRLEV   TO 102XX-EKHT-FLOVRLEV                      
026394     IF FILB-EKH-IDORDNR5 NOT NUMERIC                                     
026395       MOVE ZERO              TO 102XX-EKHT-IDORDNR5                      
026396     ELSE                                                                 
026397       MOVE FILB-EKH-IDORDNR5 TO 102XX-EKHT-IDORDNR5                      
026398     END-IF                                                               
026399     MOVE FILB-EKH-IDUSER     TO 102XX-EKHT-IDUSER                        
026400     IF FILB-EKH-PRHEMTAG NOT NUMERIC                                     
026401       MOVE ZERO              TO 102XX-EKHT-PRHEMTAG                      
026402     ELSE                                                                 
026403       MOVE FILB-EKH-PRHEMTAG TO 102XX-EKHT-PRHEMTAG                      
026404     END-IF                                                               
026405     MOVE FILB-EKH-FLDCET     TO 102XX-EKHT-FLDCET                        
026406     MOVE FILB-EKH-IDKUNDRF   TO 102XX-EKHT-IDKUNDRF                      
026407     MOVE FILB-EKH-IDFAKT-EXP TO 102XX-EKHT-IDFAKT-EXP                    
026408     MOVE FILB-EKH-CMD        TO 102XX-EKHT-CMD                           
026409                                                                          
026410     WRITE 102XX-POST   FROM 102XX-AREA                                   
026411                                                                          
026412     MOVE 'SAP'         TO POSTSUM-TRANSTYP                               
026413     MOVE 'W5104D'      TO POSTSUM-FDNAMN                                 
026414     MOVE 'W51039DB'    TO POSTSUM-DDNAMN2                                
026415     CALL POSTSUM USING POSTSUM-PARM                                      
026416     .                                                                    
026417     EJECT                                                                
026418* --- IMS SEKTIONER ---                                                   
026419                                                                          
026420 IMS-GET-WDR8   SECTION.                                                  
026421                                                                          
026422     CALL CBLTDLI USING GN FILB-PCB DLI-IO-AREA                           
026430     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
026500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
026600     PERFORM IMS-STATUSKONTROLL                                           
026700     .                                                                    
026800     SKIP3                                                                
026900 IMS-STATUSKONTROLL SECTION.                                              
027000                                                                          
027100     SET STATUS-IX TO 1                                                   
027200     SEARCH GODK-STATUS                                                   
027300       AT END                                                             
027400         MOVE 'EJ GODKÄND STATUSKOD ' TO FELTEXT-STR                      
027500         DISPLAY FELTEXT                                                  
027600         CALL FELLOG                                                      
027700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027800         CONTINUE                                                         
027900     END-SEARCH                                                           
028000     .                                                                    
