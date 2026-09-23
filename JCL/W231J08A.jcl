//W231J08A JOB (640W2310100W231J08A,W100),'RTN W231V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*                                                                             
//COPY EXEC W001HFSC,CONV='(BPXFX311)',                                         
//             DSIN=W231.W231V1.W2318A(+0),                                     
//             PATHOUT='/app/vccs/qase/w221/data/w2318A.xls'                    
//*                                                                             
//*** old path PATHOUT='/volvo/vccsroot/w221/data/w2318A.xls'                   
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W231J08A                                         
