000100 01  KRED-W6H721.                                                         
000200*                                 KVALITET                                
000300*                                 KONTROLLRAPPORT - KREDITNOTA            
000400*                                 FYSISK NYCKEL : IDSEGMNR                
000500     03 KRED-IDSEGMNR        PIC S9              COMP-3.                  
000600*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
000700*                                 SEQUENCE ORDER ID, ON SEGMENT           
000800     03 KRED-IDPTYP          PIC X(3).                                    
000900*                                 POSTTYP                                 
001000*                                 RECORD TYPE                             
001100     03 KRED-IDVERNR         PIC X(9).                                    
001200*                                 VERIFIKATIONSNUMMER                     
001300*                                 VERIFICATION NUMBER                     
001400     03 KRED-BENAEMN         PIC X(25).                                   
001500*                                 BENÄMNING                               
001600*                                 NAME                                    
001700     03 KRED-IDTFN           PIC X(20).                                   
001800*                                 TELEFONNUMMER EXTERNT                   
001900*                                 TELEPHONE NUMBER  EXTERNAL              
002000     03 KRED-TIKRED          PIC S9(7)           COMP-3.                  
002100*                                 KREDITERINGSDATUM (ÅÅMMDD)              
002200*                                 CREDIT NOTE DATE   (YYMMDD)             
002300     03 KRED-PRARTBEL-PR     PIC S9(8)V9(5)      COMP-3.                  
002400*                                 DETTA BESTÄLLNINGSPRIS                  
002500*                                 (I LEVERANTÖRENS VALUTA)                
002600     03 KRED-SUOMK-INT-5DEC  PIC S9(7)V9(5)      COMP-3.                  
002700*                                 SUMMA OMKOSTNADER                       
002800*                                 AMOUNT OF COST                          
002900     03 KRED-SUOMK-EXT-5DEC  PIC S9(7)V9(5)      COMP-3.                  
003000*                                 SUMMA OMKOSTNADER                       
003100*                                 AMOUNT OF COST                          
003200     03 KRED-SUMAT-5DEC      PIC S9(7)V9(5)      COMP-3.                  
003300*                                 MATERIALKOSTNAD                         
003400     03 KRED-PRMOMS          PIC S9(7)V9(2)      COMP-3.                  
003500*                                 MERVÄRDESSKATT                          
003600*                                 VAT                                     
003700     03 KRED-TEKREKON-INT    PIC X(70).                                   
003800*                                 NOTERING EKONOMI INTERN                 
003900*                                 NOTE ACCOUNTING INTERNAL                
004000     03 KRED-TEKREKON-EXT    PIC X(70).                                   
004100*                                 NOTERING EKONOMI EXTERN                 
004200*                                 NOTE ACCOUNTING EXTERNAL                
004300     03 KRED-FILLER          PIC X(5).                                    
004400*** END OF VILMAII-COPY LENGTH= 240 BYTES                                 
