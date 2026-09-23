000010                                                                          
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4321800.                                                
000300 AUTHOR.         STEFAN KIHLBERG.                                         
000400 DATE-WRITTEN.   00/11/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAM FÖR ATT SAMLA IHOP FILER SOM KOMMIT IN FRÅN VIPS         
001000*        GENOM KUNDREGISTER                                               
001100*                                                                         
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002501     SKIP2                                                                
002502*          --- INFIL FRÅN VIPS                                            
002503     SELECT W43218                     ASSIGN TO W43218D1.                
002504     SKIP2                                                                
002505*          --- FIL MED KUNDUPPGIFTER FRÅN VIPS                            
002510     SELECT W43220                     ASSIGN TO W43218D2.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003101     SKIP3                                                                
003102 FD  W43218                                                               
003103     RECORDING       F                                                    
003104     BLOCK CONTAINS  0.                                                   
003105                                                                          
003106*01  -COPY W43218           -L.                                           
003108     SKIP3                                                                
003109                                                                          
003110 FD  W43220                                                               
003111     RECORDING       F                                                    
003120     BLOCK CONTAINS  0.                                                   
003121                                                                          
003140*01  POST -COPY WDB701  -PRE W43220-   -L.                                
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003401                                                                          
003410*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'W4321800'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003901                                                                          
003902 77  W43218-EOF-SW               PIC X       VALUE 'N'.                   
003910     88  END-OF-W43218                       VALUE 'J'.                   
004000     EJECT                                                                
004100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004200 01  FILLER REDEFINES DAGENS-DATUM.                                       
004300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004510                                                                          
004520 01  DAGENS-DASSAAMMDD        PIC 9(8)       VALUE ZERO.                  
004530 01  DAGENS-DASSAAMMDD-GRP    REDEFINES DAGENS-DASSAAMMDD.                
004540     03 DAGENS-TISS           PIC 9(2).                                   
004550     03 DAGENS-TIAAMMDD       PIC 9(6).                                   
004560                                                                          
004600     EJECT                                                                
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800*                                                                         
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005100     SKIP2                                                                
005200*    --- PARAMETRAR TILL ABEND                                            
005300                                                                          
005400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005700     SKIP2                                                                
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101     EJECT                                                                
006102*    --- PARAMETRAR TILL POSTSUM                                          
006103*                                                                         
006110*01  -COPY W0005   -PRE  POSTSUM-                                         
006301     EJECT                                                                
006302 01  IN-AREA-START               PIC X(24)   VALUE                        
006303                                 'IN-AREA-START  '.                       
006304     SKIP2                                                                
006305*01 -COPY W43218   -PRE IN-                                               
006310                                                                          
006311     EJECT                                                                
006312 01  UT-AREA-START             PIC X(24)   VALUE                          
006313                                 'UT-AREA-START  '.                       
006315*01  -COPY WDB701                                                         
006316                                                                          
006320     SKIP2                                                                
006400     EJECT                                                                
006500 PROCEDURE DIVISION.                                                      
006600 MAIN SECTION.                                                            
006800     SKIP2                                                                
006900                                                                          
007000     PERFORM A-INIT                                                       
007110     PERFORM S01-LAES-W43218                                              
007200     PERFORM UNTIL END-OF-W43218                                          
007210        EVALUATE IN-IDPTYP                                                
007220           WHEN '001'                                                     
007400           PERFORM B-FLYTTA-DATA                                          
007410           PERFORM S11-SKRIV-W43220                                       
007500        END-EVALUATE                                                      
007910       PERFORM S01-LAES-W43218                                            
008000     END-PERFORM                                                          
008100                                                                          
008200                                                                          
008300     PERFORM Z-FINIT                                                      
008400                                                                          
008500     MOVE ZERO TO RETURN-CODE                                             
008600     GOBACK                                                               
008700     .                                                                    
008800     EJECT                                                                
008900 A-INIT SECTION.                                                          
009001                                                                          
009010     OPEN INPUT  W43218                                                   
009101                                                                          
009110     OPEN OUTPUT W43220                                                   
009200     SKIP2                                                                
009300     ACCEPT DAGENS-DATUM  FROM DATE                                       
009410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009500     .                                                                    
009600     EJECT                                                                
009610 B-FLYTTA-DATA SECTION.                                                   
009611                                                                          
009612     MOVE IN-IDDISTR             TO GMTD-IDDISTR                          
009613     MOVE IN-IDKUNDNR            TO GMTD-IDKUNDNR                         
009614     MOVE IN-IDDEALER-VIPS       TO GMTD-IDDEALER-VIPS                    
009615     MOVE IN-IDDEALER-VIPSINV    TO GMTD-IDDEALER-VIPSINV                 
009616                                                                          
009617     MOVE IN-BEDEALER-VIPSINV    TO GMTD-BEDEALER-VIPSINV                 
009618     MOVE IN-ADDEALER-INVRAD1    TO GMTD-ADDEALER-INVRAD1                 
009619     MOVE IN-ADDEALER-INVRAD2    TO GMTD-ADDEALER-INVRAD2                 
009620     MOVE IN-ADPOSTNR-INV        TO GMTD-ADPOSTNR-INV                     
009621     MOVE IN-ADCITY-INV          TO GMTD-ADCITY-INV                       
009622                                                                          
009623     MOVE IN-BEDEALER-VIPSGMT    TO GMTD-BEDEALER-VIPSGMT                 
009624     MOVE IN-ADDEALER-GMTRAD1    TO GMTD-ADDEALER-GMTRAD1                 
009625     MOVE IN-ADDEALER-GMTRAD2    TO GMTD-ADDEALER-GMTRAD2                 
009626     MOVE IN-ADPOSTNR-GMT        TO GMTD-ADPOSTNR-GMT                     
009627     MOVE IN-ADCITY-GMT          TO GMTD-ADCITY-GMT                       
009628                                                                          
009629     MOVE IN-KDKNDSTA            TO GMTD-KDKNDSTA                         
009630     MOVE IN-DAREGDAT            TO GMTD-DAREGDAT                         
009631     MOVE ZERO                   TO GMTD-DASTADAT                         
009632                                    GMTD-DASTODAT                         
009633     MOVE IN-KDCREDIT            TO GMTD-KDCREDIT                         
009634     MOVE IN-KDKNDKAT            TO GMTD-KDKNDKAT                         
009635                                                                          
009636     MOVE IN-IDLANDX2            TO GMTD-IDLANDX2                         
009637     MOVE IN-FLDIRAFF            TO GMTD-FLDIRAFF                         
009638     MOVE IN-DATRADAT            TO DAGENS-DASSAAMMDD                     
009639     MOVE DAGENS-TIAAMMDD        TO GMTD-TIREGDAT                         
009641     .                                                                    
009650     EJECT                                                                
009700 Z-FINIT SECTION.                                                         
009801     CLOSE W43218                                                         
009810           W43220                                                         
009901     SKIP2                                                                
009902     MOVE 'S' TO POSTSUM-OPKOD                                            
009910     CALL POSTSUM USING POSTSUM-PARM                                      
010000     .                                                                    
010101     EJECT                                                                
010102 S01-LAES-W43218  SECTION.                                                
010103     READ W43218 INTO IN-W43218                                           
010104     AT END                                                               
010106        SET END-OF-W43218 TO TRUE                                         
010107                                                                          
010108     NOT AT END                                                           
010109        MOVE 'W43218' TO POSTSUM-FDNAMN                                   
010110        MOVE 'W43218D1' TO POSTSUM-DDNAMN2                                
010113        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
010114        CALL POSTSUM USING POSTSUM-PARM                                   
010115     END-READ                                                             
010120     .                                                                    
010201     EJECT                                                                
010202 S11-SKRIV-W43220 SECTION.                                                
010203                                                                          
010204     WRITE W43220-POST FROM GMTD-WDB701                                   
010205                                                                          
010207     MOVE 'W43220' TO POSTSUM-FDNAMN                                      
010208     MOVE 'W43218D2' TO POSTSUM-DDNAMN2                                   
010209     CALL POSTSUM USING POSTSUM-PARM                                      
010210     .                                                                    
010400     EJECT                                                                
010500 S99-ABEND SECTION.                                                       
010600                                                                          
010701     SKIP2                                                                
010702     MOVE 'S' TO POSTSUM-OPKOD                                            
010710     CALL POSTSUM USING POSTSUM-PARM                                      
010800     CALL ABEND USING RKOD-ABEND                                          
010900     .                                                                    
