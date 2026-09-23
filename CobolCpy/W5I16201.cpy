000100 01  MID-W5I16201.                                                        
000200*                                 MID-COPYTEXT FÖR BILD  5162             
000300*                                 LOG BALANCE SELECTION                   
000400     03 MID-IDARTNR-IN       PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC 9(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MID-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-IDHUVTYP-IN      PIC X(4).                                    
001300*                                 LOGGTYP EKONOMISK HÄNDELSE              
001400     03 MID-IDHUVTYP-UT      PIC X(4).                                    
001500*                                 LOGGTYP EKONOMISK HÄNDELSE              
001600     03 MID-IDSUBTYP-IN      PIC X(3).                                    
001700*                                 LOGGTYP EKONOMISK HÄNDELSE              
001800     03 MID-IDSUBTYP-UT      PIC X(3).                                    
001900*                                 LOGGTYP EKONOMISK HÄNDELSE              
002000     03 MID-TIREGDAT-IN1     PIC 9(6).                                    
002100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002200     03 MID-TIREGDAT-UT1     PIC 9(6).                                    
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400     03 MID-TIREGDAT-IN2     PIC 9(6).                                    
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600     03 MID-TIREGDAT-UT2     PIC 9(6).                                    
002700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002800     03 MID-IDTRANS-IN       PIC X(4).                                    
002900*                                 BILDNUMMER                              
003000     03 MID-IDTRANS-UT       PIC X(4).                                    
003100*                                 BILDNUMMER                              
003200     03 MID-FLEXTRAKT        PIC X.                                       
003300*                                 ALLMÄN FLAGGA                           
003400     03 MID-INPUT            OCCURS 11 TIMES.                             
003500        05 MID-SELECT-RAD    PIC X.                                       
003600*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
