000010*** EDIT ALLOWED                                                          
007511*                                                                         
007513*    TABELL FÖR OMVANDLING VOLVO-VALUTAKOD OCH ISO-VALUTAKOD.             
007514*    ÖVERSÄTTNING SKER I W93001 MED KURSER FRÅN REBUS.                    
007515*                                                                         
007516 01  VALUTAKOD-TABELL.                                                    
007517   03  TAB-IX                  PIC S9(9)   VALUE ZERO COMP SYNC.          
007518   03  TAB-IX-MAX              PIC S9(9)   VALUE +44  COMP SYNC.          
007519*                                                                         
007520   03  TAB-INFO.                                                          
007530     05  FILLER                PIC X(7)    VALUE '001SEKN'.               
007531     05  FILLER                PIC X(7)    VALUE '002USDN'.               
007532     05  FILLER                PIC X(7)    VALUE '003GBPN'.               
007533     05  FILLER                PIC X(7)    VALUE '004DEMN'.               
007534     05  FILLER                PIC X(7)    VALUE '005DKKN'.               
007535     05  FILLER                PIC X(7)    VALUE '006CHFN'.               
007536     05  FILLER                PIC X(7)    VALUE '007PTEN'.               
007537     05  FILLER                PIC X(7)    VALUE '008ATSN'.               
007538     05  FILLER                PIC X(7)    VALUE '009NOKN'.               
007539     05  FILLER                PIC X(7)    VALUE '010FIMN'.               
007540     05  FILLER                PIC X(7)    VALUE '011FRFN'.               
007541     05  FILLER                PIC X(7)    VALUE '012ITLN'.               
007542     05  FILLER                PIC X(7)    VALUE '013NLGN'.               
007543     05  FILLER                PIC X(7)    VALUE '014BEFN'.               
007544     05  FILLER                PIC X(7)    VALUE '015BRNN'.               
007545     05  FILLER                PIC X(7)    VALUE '016AUDN'.               
007546     05  FILLER                PIC X(7)    VALUE '017CADN'.               
007547     05  FILLER                PIC X(7)    VALUE '018PLNN'.               
007548     05  FILLER                PIC X(7)    VALUE '019DDMN'.               
007549     05  FILLER                PIC X(7)    VALUE '020ESPN'.               
007550     05  FILLER                PIC X(7)    VALUE '021YUDN'.               
007551     05  FILLER                PIC X(7)    VALUE '022HUFN'.               
007552     05  FILLER                PIC X(7)    VALUE '023CSKN'.               
007553     05  FILLER                PIC X(7)    VALUE '024JPYN'.               
007554     05  FILLER                PIC X(7)    VALUE '025IEPN'.               
007555     05  FILLER                PIC X(7)    VALUE '026MYRN'.               
007556     05  FILLER                PIC X(7)    VALUE '027ZARN'.               
007557     05  FILLER                PIC X(7)    VALUE '028LBPN'.               
007558     05  FILLER                PIC X(7)    VALUE '029SARN'.               
007559     05  FILLER                PIC X(7)    VALUE '030INRN'.               
007560     05  FILLER                PIC X(7)    VALUE '031SGDN'.               
007561     05  FILLER                PIC X(7)    VALUE '032HKDN'.               
007562     05  FILLER                PIC X(7)    VALUE '033GRDN'.               
007563     05  FILLER                PIC X(7)    VALUE '034THBN'.               
007564     05  FILLER                PIC X(7)    VALUE '035PEIN'.               
007565     05  FILLER                PIC X(7)    VALUE '036CLPN'.               
007566     05  FILLER                PIC X(7)    VALUE '037ECUN'.               
007567     05  FILLER                PIC X(7)    VALUE '038ECUN'.               
007568     05  FILLER                PIC X(7)    VALUE '039JPYN'.               
007569     05  FILLER                PIC X(7)    VALUE '040ECUN'.               
007570     05  FILLER                PIC X(7)    VALUE '041JPYN'.               
007571     05  FILLER                PIC X(7)    VALUE '042RURN'.               
007572     05  FILLER                PIC X(7)    VALUE '043KRWN'.               
007573     05  FILLER                PIC X(7)    VALUE '044EURN'.               
007574   03  TABELL REDEFINES TAB-INFO OCCURS 44.                               
007575     05  TAB-KDVALUTA          PIC 9(3).                                  
007576     05  TAB-KDVALISO          PIC X(3).                                  
007577     05  TAB-FLAGGA            PIC X(1).                                  
