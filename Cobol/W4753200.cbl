001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4753200.                                                
001300*AUTHOR.         CAMELIA OLGRENER.                                        
001400*DATE-WRITTEN.   93/08/25.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001810*        PROGRAM SOM SKAPAR EN FIL MED FAKTURERADE KOLLIN                 
001820*        SOM EJ VALTS TILL TULLEN.                                        
001830*                                                                         
001900*        WDM7 LÄSES NER MED SB.                                           
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000 SKIP3                                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401                                                                          
003402*          --- UTFIL MED FAKT KOLLIN SOM EJ VALTS TILL TULLEN             
003410     SELECT W47532                     ASSIGN TO W47532D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800                                                                          
003900 FILE SECTION.                                                            
004001                                                                          
004002 FD  W47532                                                               
004003     RECORDING       F                                                    
004010     BLOCK CONTAINS  0.                                                   
004020 01 UTPOST  -COPY W4753201    -L.                                         
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4753200'.            
004900     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  UT-AREA-START               PIC X(24)   VALUE                        
007303                                 'UT-AREA-START  '.                       
007310                                                                          
007311 01  UT-AREA.                                                             
007312     03  -COPY W4753201   -PRE UT-                                        
007400     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900                                                                          
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  BASEN-SLUT                          VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010000     SKIP3                                                                
010100 01  DLI-IO-AREA.                                                         
010200     03  IO-AREA                 PIC X(3000)  VALUE SPACE.                
010301                                                                          
010302     03  FILLER REDEFINES IO-AREA.                                        
010303*        05  -COPY WDM701                                                 
010304                                                                          
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800                                                                          
010902*01  -COPY W0008  -PRE WDM7-                                              
010910     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011101 PROCEDURE DIVISION  USING WDM7-PCB.                                      
011110     ENTRY 'DLITCBL' USING WDM7-PCB.                                      
011200                                                                          
011500     PERFORM A-INIT                                                       
011510                                                                          
011600     PERFORM IMS-GN-WDM7                                                  
011700     PERFORM UNTIL BASEN-SLUT                                             
011701                                                                          
011710       IF HUV-IDTULLNR = 0                                                
011720         PERFORM B-SKAPA-POST                                             
011731       END-IF                                                             
011732       PERFORM IMS-GN-WDM7                                                
011734                                                                          
011735     END-PERFORM                                                          
011736                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013601                                                                          
013610     OPEN OUTPUT W47532                                                   
013700                                                                          
013910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014100     .                                                                    
014200     EJECT                                                                
014210 B-SKAPA-POST SECTION.                                                    
014220                                                                          
014230     MOVE HUV-IDFAKT   TO UT-IDFAKT                                       
014240     MOVE HUV-IDDISTR  TO UT-IDDISTR                                      
014250     MOVE HUV-IDKUNDNR TO UT-IDKUNDNR                                     
014260     MOVE HUV-IDORDNR7 TO UT-IDORDNR7                                     
014261     MOVE HUV-IDPRODNR TO UT-IDPRODNR                                     
014270     MOVE HUV-IDKOLLI  TO UT-IDKOLLI                                      
014280     MOVE HUV-TIFAKT   TO UT-TIFAKT                                       
014290     MOVE HUV-KDORDKL  TO UT-KDORDKL                                      
014291                                                                          
014292     PERFORM S01-SKRIV-W47532                                             
014293     .                                                                    
014294     EJECT                                                                
014300 Z-FINIT SECTION.                                                         
014400                                                                          
014410     CLOSE W47532                                                         
014501                                                                          
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014801     EJECT                                                                
014802 S01-SKRIV-W47532 SECTION.                                                
014803                                                                          
014804     WRITE UTPOST FROM UT-AREA                                            
014805                                                                          
014806     MOVE 'FAK'     TO POSTSUM-TRANSTYP                                   
014807     MOVE 'W47532' TO POSTSUM-FDNAMN                                      
014808     MOVE 'W47532D1' TO POSTSUM-DDNAMN2                                   
014809     CALL POSTSUM USING POSTSUM-PARM                                      
014810     .                                                                    
015000     EJECT                                                                
015700* --- IMS SEKTIONER ---                                                   
015800     SKIP3                                                                
015902 IMS-GN-WDM7   SECTION.                                                   
015903                                                                          
015904     CALL CBLTDLI USING GN WDM7-PCB DLI-IO-AREA                           
015905     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
015906     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015907     PERFORM IMS-STATUSKONTROLL                                           
015910     .                                                                    
016000     SKIP3                                                                
016100 IMS-STATUSKONTROLL SECTION.                                              
016200                                                                          
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GODK-STATUS                                                   
016500       AT END                                                             
016600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016700         DISPLAY FELTEXT                                                  
016800         CALL FELLOG                                                      
016900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
