000100 01  DCQ-W6D221.                                                          
000200*                                 KVALITETSKONTROLL                       
000300*                                 ARTIKELINFO FÖR SDC/NDC                 
000400*                                 FYSISK NYCKEL: IDDC                     
000500     03 DCQ-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 DCQ-TISTADAT         PIC S9(7)           COMP-3.                  
000900*                                 GENERELLT STARTDATUM                    
001000*                                 GENERAL START DATE                      
001100     03 DCQ-TISTODAT         PIC S9(7)           COMP-3.                  
001200*                                 GENERELLT STOPPDATUM                    
001300*                                 GENERAL STOP DATE YYMMDD                
001400     03 DCQ-KVANTAL          PIC S9(7)           COMP-3.                  
001500*                                 ANTAL                                   
001600*                                 NUMBER                                  
001700     03 DCQ-KVAVV-KVAL       PIC S9(7)           COMP-3.                  
001800*                                 ANTALSAVVIKELSE KVALITET                
001900*                                 QUANTITYDEVIATION QUALITY               
002000     03 DCQ-KVART-SKROT      PIC S9(7)           COMP-3.                  
002100*                                 ANTAL SKROTADE ARTIKLAR                 
002200*                                 QUANTITY INSPECTED PARTS                
002300     03 DCQ-KVART-RET        PIC S9(7)           COMP-3.                  
002400*                                 ANTAL ARTIKLAR I RETUR                  
002500*                                 QUANTITY INSPECTED PARTS                
002600     03 DCQ-KVART-KJUST      PIC S9(7)           COMP-3.                  
002700*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
002800*                                 QUANTITY INSPECTED PARTS                
002900     03 DCQ-BEINIT           PIC X(5).                                    
003000*                                 INITALER FÖR EN PERSON                  
003100*                                 INITIALS FOR A PERSON                   
003200     03 DCQ-IDNAMN           PIC X(40).                                   
003300*                                 NAMN                                    
003400     03 DCQ-TEKVAINF         OCCURS 4 TIMES                               
003500                             PIC X(79).                                   
003600*                                 KVALITETS INFORMATION                   
003700*                                 QUALITY INFORMATION PART NUMBER         
003800     03 DCQ-FLSLUT           PIC X.                                       
003900*                                 AVSLUTNINGSFLAGGA                       
004000*** END OF VILMAII-COPY LENGTH= 392 BYTES                                 
