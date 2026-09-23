000100 01  4494-WDGX4494.                                                       
000200*                                 TRANSPORTINFO. BOLLA                    
000300*                                 BOLLA INFO.                             
000400*                                 FYSISK NYCKEL: KY4494                   
000500*                                  (DALASTN + IDTRPTNR                    
000600*                                  + IDZON + IDGMTREF                     
000700*                                  + IDTRPBO)                             
000800*                                 SÖKBEGREPP: NYCKLAR +                   
000900*                                  IDLBBET + IDKOLLI                      
001000     03 4494-DALASTN         PIC 9(8).                                    
001100*                                 LASTNINGSDATUM       (ÅÅÅÅMMDD)         
001200*                                 LOADING DATE         (YYYYMMDD)         
001300     03 4494-IDTRPTNR        PIC S9(3)           COMP-3.                  
001400*                                 TRANSPORTIDENTITET                      
001500*                                 TRANSPORT IDENTITY                      
001600     03 4494-IDZON           PIC X(2).                                    
001700*                                 TRANSPORTVÄG (RUTT,ZON)                 
001800*                                 TRANSPORT ROUTE (ZONE)                  
001900     03 4494-IDGMTREF.                                                    
002000*                                 GODSMOTTAGAREREFERENS                   
002100*                                 GOODS RECEIVER REFERENS                 
002200        05 4494-IDDISTR      PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400*                                 DISTRICT NUMBER                         
002500        05 4494-IDKUNDNR     PIC S9(7)           COMP-3.                  
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800        05 4494-IDKUNDRF-GRP.                                             
002900*                                 KUNDENS REFERENS (ORDERID)              
003000*                                 CUSTOMER REFERENCE (ORDER ID)           
003100           07 4494-IDKUNDRF  PIC X(10).                                   
003200*                                 KUNDENS REFERENS (ORDERID)              
003300*                                 CUSTOMER REFERENCE (ORDER ID)           
003400           07 4494-IDORDNR5-FILLER REDEFINES 4494-IDKUNDRF.               
003500              09 4494-IDORDNR5                                            
003600                             PIC 9(5).                                    
003700*                                 ORDERNUMMER                             
003800*                                 ORDER NUMBER                            
003900              09 FILLER      PIC X(5).                                    
004000           07 4494-IDORDNR7-FILLER REDEFINES 4494-IDKUNDRF.               
004100              09 4494-IDORDNR7                                            
004200                             PIC 9(7).                                    
004300*                                 ORDERNUMMER                             
004400*                                 ORDER NUMBER                            
004500              09 FILLER      PIC X(3).                                    
004600     03 4494-IDTRPBO.                                                     
004700*                                 BOLLA-DOKUMENT IDENTITET                
004800*                                 TRANSPORT BOLLA DOCUMENT IDENT.         
004900        05 4494-IDTRPBOT     PIC X.                                       
005000*                                 BOLLA-DOKUMENT TECKEN                   
005100*                                 TRANSPORT BOLLA DOCUMENT LETTER         
005200        05 4494-IDTRPBON     PIC S9(7)           COMP-3.                  
005300*                                 BOLLA-DOKUMENT NUMMER                   
005400*                                 TRANSPORT BOLLA DOCUMENT NO.            
005500     03 4494-IDLBBET         PIC X(12).                                   
005600*                                 LASTBÄRARBETECKNING                     
005700*                                 TRAILER NUMBER                          
005800     03 4494-IDTRPBOR        PIC S9(5)           COMP-3.                  
005900*                                 BOLLA-DOKUMENT REFERENSNUMMER           
006000*                                 BOLLA DOCUMENT REFERENCE NO.            
006100     03 4494-KVKOLLI         PIC S9(5)           COMP-3.                  
006200*                                 ANTAL KOLLI                             
006300*                                 NBR OF CASES                            
006400     03 4494-TIORDREG        PIC S9(7)           COMP-3.                  
006500*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
006600*                                 ORDER REGISTRATION DATE  YYMMDD         
006700     03 4494-VKORDBTO        PIC S9(6)V9(1)      COMP-3.                  
006800*                                 ORDERVIKT BRUTTO (KG)                   
006900*                                 GROSS WEIGHT (KG)                       
007000     03 4494-VLORDBTO        PIC S9(4)V9(3)      COMP-3.                  
007100*                                 ORDERVOLYM BRUTTO (M3)                  
007200*                                 GROSS VOLUME PER ORDER (M3)             
007300     03 4494-IDTRPTNR-GRUND  PIC S9(3)           COMP-3.                  
007400*                                 TRANSPORTIDENTITET GRUNDVÄRDE           
007500*                                 TRANSPORT IDENTITY START VALUE          
007600     03 4494-IDKOLLI         PIC S9(5)           COMP-3.                  
007700*                                 KOLLINUMMER                             
007800*                                 CASE NUMBER                             
007900     03 4494-IDPRODNR        PIC S9(7)           COMP-3.                  
008000*                                 PRODUKTIONSNUMMER                       
008100*                                 PRODUCTION NUMBER                       
008200*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
