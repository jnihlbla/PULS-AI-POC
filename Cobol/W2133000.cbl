000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2133000.                                    
000300 AUTHOR.                     IDK,GÖTEBORG.                                
000400 DATE-WRITTEN.               NOVEMBER 1978.                               
000500                                                                          
000600*REMARKS.                                                                 
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        PROGRAMMMET TÖMMER HÄNDELSEREGISTRET WLXXBN (WDG3),              
001000*        DVS LÄSER OCH DELETAR HTR 2221/2222                              
001100*        PROGRAMMET ÄR EN BMP UTAN CHECK-POINTS. OM DET                   
001200*        FINNS ALLTFÖR MÅNGA HÄNDELSETRANSAR, AVSLUTAS PROGRAMMET         
001300*        OCH SKRIVER EN POST PÅ EN HJÄLP-FIL, VILKET LEDER TILL           
001400*        ATT JOBBET BESTÄLLS OCH KÖRS YTTERLIGARE EN GÅNG.                
001500*        (OM ALLA TRANSAR FÖRBRUKATS BLIR HJÄLP-FILEN TOM)                
001600*                                                                         
001700*    SUBPROGRAM.                                                          
001800*        W2133010    SUBPROGRAM SOM SKÖTER SAMTLIGA IMS-CALL              
001900*                    MOT HÄNDELSEREGISTRET.                               
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*--------------------------------------- TRANSAKTIONER FÖR                
002600*                                        LEVERANTÖRSBYTE                  
002700*                                        OUTPUT                           
002800     SELECT W21331            ASSIGN W21330D1.                            
002900     SKIP2                                                                
003000*--------------------------------------- HJÄLP-FIL                        
003100     SELECT W21330            ASSIGN W21330D2.                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500     SKIP2                                                                
003600 FD  W21331                                                               
003700     RECORDING F                                                          
003800     BLOCK 0.                                                             
003900                                                                          
004000*01  POST -COPY W213R01    -PRE U31- -L.                                  
004100     SKIP3                                                                
004200 FD  W21330                                                               
004300     RECORDING F                                                          
004400     BLOCK 0.                                                             
004500                                                                          
004600 01  W21330-POST        PIC X(10).                                        
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP3                                                                
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 01  RKOD                    PIC S9(4)               COMP SYNC.           
005300 01  DELETE-ANT              PIC S9(5)               COMP-3.              
005400     SKIP3                                                                
005500 01  KONSTANTER.                                                          
005600     05  JA                  PIC X       VALUE 'J'.                       
005700     05  NEJ                 PIC X       VALUE 'N'.                       
005800     05  PROGRAM-NAMN        PIC X(6)    VALUE 'W21330'.                  
005900     05  LAES-ROT            PIC S9(3)   VALUE +101  COMP-3.              
006000     05  LAES-DELETE-SEGM3   PIC S9(3)   VALUE +102  COMP-3.              
006100     SKIP3                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
006400     05  W2133010            PIC X(8)    VALUE 'W2133010'.                
006500     EJECT                                                                
006600*--------------------------------------- PARAMETRAR TILL POSTSUM          
006700*                                                                         
006800*01  -COPY W0005      -PRE POSTSUM-                                       
006900     EJECT                                                                
007000*--------------------------------------- AREA FÖR W21331-POST             
007100*                                                                         
007200                                                                          
007300*01  AREA -COPY W213R01    -PRE U31-                                      
007400     EJECT                                                                
007500 01  U30-AREA.                                                            
007600     03  FILLER            PIC X(10)    VALUE 'KÖR IGEN!!'.               
007700     EJECT                                                                
007800*--------------------------------------- LÄNKAREA FÖR CALL MOT            
007900*                                        HÄNDELSEREGISTRET                
008000                                                                          
008100*01   -COPY W213L301   -PRE IMSWDG3-                                      
008200     EJECT                                                                
008300 LINKAGE SECTION.                                                         
008400     SKIP2                                                                
008500*01  -COPY W0009  -PRE MSG-                                               
008600     EJECT                                                                
008700*01  -COPY W0008  -PRE  XXBN-                                             
008800     05  FILLER             PIC X.                                        
008900     EJECT                                                                
009000 PROCEDURE DIVISION  USING MSG-PCB XXBN-PCB.                              
009100 HUVUD SECTION.                                                           
009200     ENTRY 'DLITCBL' USING MSG-PCB XXBN-PCB.                              
009300     SKIP2                                                                
009400     PERFORM A-INITIERING                                                 
009500                                                                          
009600     MOVE LAES-ROT TO IMSWDG3-KDCALL                                      
009700     MOVE '2221' TO IMSWDG3-IDHTYP                                        
009800     CALL W2133010 USING IMSWDG3-W213L301 XXBN-PCB                        
009900                                                                          
010000     IF  IMSWDG3-ANROP-OK                                                 
010100       MOVE LAES-DELETE-SEGM3 TO IMSWDG3-KDCALL                           
010200       CALL W2133010 USING IMSWDG3-W213L301 XXBN-PCB                      
010300       MOVE +1 TO DELETE-ANT                                              
010400                                                                          
010500       PERFORM UNTIL IMSWDG3-ANROP-FEL OR DELETE-ANT > 500                
010600         MOVE IMSWDG3-IDARTNR  TO U31-IDARTNR                             
010700         MOVE IMSWDG3-IDLEVNR  TO U31-IDLEVNR                             
010800                                  U31-IDLEVNR-SHIP                        
010900         MOVE IMSWDG3-IDSYSTEM TO U31-IDSYSTEM                            
011000         PERFORM B-SKRIV-W21331                                           
011100         CALL W2133010 USING IMSWDG3-W213L301 XXBN-PCB                    
011200         ADD +1 TO DELETE-ANT                                             
011300       END-PERFORM                                                        
011400     END-IF                                                               
011500                                                                          
011600     IF IMSWDG3-ANROP-OK                                                  
011700*      -- DET FINNS MER HÄNDELSER ATT LÄSA --                             
011800       WRITE W21330-POST FROM U30-AREA                                    
011900     END-IF                                                               
012000                                                                          
012100     PERFORM Z-AVSLUTNING                                                 
012200     MOVE ZERO TO RETURN-CODE                                             
012300     GOBACK                                                               
012400     .                                                                    
012500     EJECT                                                                
012600******************************************************************        
012700*                                                                *        
012800*    INITIERING                                                  *        
012900*    ÖPPNA ALLA FILER                                            *        
013000*                                                                *        
013100******************************************************************        
013200                                                                          
013300 A-INITIERING SECTION.                                                    
013400                                                                          
013500     OPEN OUTPUT W21331 W21330                                            
013600                                                                          
013700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
013800     MOVE 'RO1' TO U31-IDPTYP                                             
013900     .                                                                    
014000     EJECT                                                                
014100******************************************************************        
014200*                                                                *        
014300*    SKRIV W21331-POST                                           *        
014400*                                                                *        
014500******************************************************************        
014600                                                                          
014700 B-SKRIV-W21331 SECTION.                                                  
014800     SKIP2                                                                
014900     WRITE U31-POST FROM U31-AREA                                         
015000                                                                          
015100     MOVE 'W21331'   TO POSTSUM-FDNAMN                                    
015200     MOVE 'W21330D1' TO POSTSUM-DDNAMN2                                   
015300     MOVE '2221'     TO POSTSUM-TRANSTYP                                  
015400     CALL POSTSUM USING POSTSUM-PARM                                      
015500     .                                                                    
015600     EJECT                                                                
015700******************************************************************        
015800*                                                                *        
015900*    AVSLUTNING                                                  *        
016000*    STÄNG ALLA FILER                                            *        
016100*    SKRIV UT POSTSUMS RÄKNEVERK                                 *        
016200*                                                                *        
016300******************************************************************        
016400                                                                          
016500 Z-AVSLUTNING SECTION.                                                    
016600                                                                          
016700     CLOSE W21331 W21330                                                  
016800                                                                          
016900     MOVE 'S' TO POSTSUM-OPKOD                                            
017000     CALL POSTSUM USING POSTSUM-PARM                                      
017100     .                                                                    
