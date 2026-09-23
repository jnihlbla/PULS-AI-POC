//W114Z5SE JOB (640W1140100W114Z5SE,W100),'RTN W114D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*        THE W114Z5SE JCL IS USING THE W114Z2SE PARTNER                       
//*                                                                             
//*** DC71                                                                      
//TOM1    EXEC WEMPTST,DSIN=WUT.W114D5.W1145C(+0)                               
//VCOM1    EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM1.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W1145C(+0),DISP=SHR                         
//*                                                                             
//*** DC72                                                                      
//TOM2    EXEC WEMPTST,DSIN=WUT.W114D5.W1145D(+0)                               
//VCOM2    EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM2.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W1145D(+0),DISP=SHR                         
//*                                                                             
//*** DC41                                                                      
//TOM3    EXEC WEMPTST,DSIN=WUT.W114D5.W1145F(+0)                               
//VCOM3    EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM3.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W1145F(+0),DISP=SHR                         
//*                                                                             
//*** DC43                                                                      
//TOM4    EXEC WEMPTST,DSIN=WUT.W114D5.W1145G(+0)                               
//VCOM4    EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM4.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W1145G(+0),DISP=SHR                         
//*                                                                             
//*** DC44                                                                      
//TOM5    EXEC WEMPTST,DSIN=WUT.W114D5.W1145H(+0)                               
//VCOM5    EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM5.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W1145H(+0),DISP=SHR                         
//*                                                                             
//*** DC45                                                                      
//TOM6    EXEC WEMPTST,DSIN=WUT.W114D5.W1145I(+0)                               
//VCOM6    EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM6.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W1145I(+0),DISP=SHR                         
//*                                                                             
//*** DC46                                                                      
//TOM7    EXEC WEMPTST,DSIN=WUT.W114D5.W1145J(+0)                               
//VCOM7    EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM7.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W1145J(+0),DISP=SHR                         
//*                                                                             
//*** DC47                                                                      
//TOM8    EXEC WEMPTST,DSIN=WUT.W114D5.W1145K(+0)                               
//VCOM8    EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM8.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W1145K(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114Z5SE                                         
