000100 01  INFO-W6D211.                                                         
000200*                                 KVALITETSKONTROLL                       
000300*                                 ARTIKELINFO SEGMENT                     
000400*                                 FYSISK NYCKEL: W6D211KY:                
000500*                                 (DAREGDAT-9KOMPL +                      
000600*                                  TIKLOCK-9KOMPL)                        
000700     03 INFO-DAREGDAT-9KOMPL PIC 9(8).                                    
000800*                                 DATUMETS 9-KOMPLEMENT                   
000900*                                 DATES 9-COMPLEMENT                      
001000     03 INFO-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
001100*                                 TID LAGRAT SOM 9-KOMPLEMENT             
001200*                                 TIME SAVED AS 9-COMPLEMENT              
001300     03 INFO-KDPERSON        PIC S9(3)           COMP-3.                  
001400*                                 PERSONKOD                               
001500*                                 STAFF CODE                              
001600     03 INFO-KDKVAINF        PIC X.                                       
001700*                                 TYP AV KVAL.INFO F÷R ARTIKEL            
001800*                                 TYPE OF QUAL.INFO. FOR PART             
001900     03 INFO-IDKVAINF        PIC 9(2).                                    
002000*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
002100*                                 LINENO FOR QUALITY CONTROL TEXT         
002200     03 INFO-TEKVAINP        PIC X(20).                                   
002300*                                 KVALITETS INFORMATION ARTIKEL-I         
002400*                                 NPUT                                    
002500*                                 QUALITY INFORMATION PART-INPUT          
002600     03 INFO-TIKLAR-QUAL     PIC S9(7)           COMP-3.                  
002700*                                 KLARDATUM          (≈≈MMDD)             
002800*                                 READY DATE        (YYMMDD)              
002900     03 INFO-FLQPA           PIC X.                                       
003000*                                 QUALITY POINT ASSURED FLAGGA            
003100*                                                                         
003200*                                 QUALITY POINT ASSURED FLAG              
003300*                                                                         
003400     03 INFO-TEKVAINF-INT    OCCURS 7 TIMES                               
003500                             PIC X(79).                                   
003600*                                 KVALITETS INFORMATION INTERNT           
003700*                                 QUALITY INFORMATION PART NUMBER         
003800*                                  EXTERNAL                               
003900     03 INFO-TIKLAR-LEV      PIC S9(7)           COMP-3.                  
004000*                                 KLARDATUM          (≈≈MMDD)             
004100*                                 READY DATE        (YYMMDD)              
004200     03 INFO-FLSTOCH         PIC X.                                       
004300*                                 STOCKCHECK FLAGGA                       
004400*                                                                         
004500*                                 STOCKCHECK FLAG                         
004600*                                                                         
004700     03 INFO-TEKVAINF-EXT    OCCURS 7 TIMES                               
004800                             PIC X(79).                                   
004900*                                 KVALITETS INFORMATION EXTERNT           
005000*                                 QUALITY INFORMATION PART NUMBER         
005100*                                  EXTERNAL                               
005200     03 INFO-TIREGDAT        PIC S9(7)           COMP-3.                  
005300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005400*                                 REGISTRATION DATE (YYMMDD)              
005500*** END OF VILMAII-COPY LENGTH= 1158 BYTES                                
