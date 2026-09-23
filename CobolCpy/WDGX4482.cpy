000100 01  4482-WDGX4482.                                                       
000200*                                 LASTBÄRARE PROFORMA                     
000300*                                 KOLLI                                   
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 ( IDDISTR  + IDKUNDNR +                 
000600*                                   KDFAKTYP            +                 
000700*                                   IDORDNR7 + IDPRODNR +                 
000800*                                   IDKOLLI)                              
000900     03 4482-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200     03 4482-IDKUNDNR        PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500     03 4482-KDFAKTYP        PIC X.                                       
001600*                                 FAKTURATYP                              
001700*                                 INVOICE TYPE                            
001800     03 4482-IDKUNDRF        PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*                                 CUSTOMER REFERENCE (ORDER ID)           
002100     03 4482-IDORDNR5-FILLER REDEFINES 4482-IDKUNDRF.                     
002200        05 4482-IDORDNR5     PIC 9(5).                                    
002300*                                 ORDERNUMMER                             
002400*                                 ORDER NUMBER                            
002500        05 FILLER            PIC X(5).                                    
002600     03 4482-IDORDNR7-FILLER REDEFINES 4482-IDKUNDRF.                     
002700        05 4482-IDORDNR7     PIC 9(7).                                    
002800*                                 ORDERNUMMER                             
002900*                                 ORDER NUMBER                            
003000        05 FILLER            PIC X(3).                                    
003100     03 4482-IDPRODNR        PIC S9(7)           COMP-3.                  
003200*                                 PRODUKTIONSNUMMER                       
003300*                                 PRODUCTION NUMBER                       
003400     03 4482-IDKOLLI         PIC S9(5)           COMP-3.                  
003500*                                 KOLLINUMMER                             
003600*                                 CASE NUMBER                             
003700     03 4482-IDDEALER        PIC S9(7)           COMP-3.                  
003800*                                 DEALER KUNDNUMMER                       
003900     03 4482-FILLER          PIC X(3).                                    
004000     03 4482-IDPSN           OCCURS 2 TIMES                               
004100                             PIC 9(3).                                    
004200*                                 PROPER SHIPPING NAME                    
004300*                                 PROPER SHIPPING NAME                    
004400     03 4482-KDKOLLI         PIC X(8).                                    
004500*                                 KOLLIKOD                                
004600*                                 KOLLI CODE                              
004700     03 4482-TIRFS           PIC S9(11)          COMP-3.                  
004800*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
004900*                                 READY FOR SHIPMENT  YYMMDDHHMM          
005000     03 4482-VKORDBTO        PIC S9(6)V9(1)      COMP-3.                  
005100*                                 ORDERVIKT BRUTTO (KG)                   
005200*                                 GROSS WEIGHT (KG)                       
005300     03 4482-VLORDBTO        PIC S9(4)V9(3)      COMP-3.                  
005400*                                 ORDERVOLYM BRUTTO (M3)                  
005500*                                 GROSS VOLUME PER ORDER (M3)             
005600     03 4482-FILLER          PIC X(15).                                   
005700*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
