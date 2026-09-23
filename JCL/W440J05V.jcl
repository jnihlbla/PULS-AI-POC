//W440J05V JOB (640W4400100W440J05V,W100),'RTN W440V4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//*+JBS BIND IMG0                                                               
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W440    EXEC W440P05V                                                         
//*                                                                             
//COPY1   EXEC W001HFSC,CONV='(BPXFX311)',                                      
//             DSIN='W440.W440V4.W4405X(+1)',                                   
//             PATHOUT='/app/vccs/qase/w440/data/w4405x.xls'                    
//*                                                                             
//*** old path PATHOUT='/volvo/vccsroot/w440/data/w4405x.xls'                   
//*                                                                             
//COPY2   EXEC W001HFSC,CONV='(BPXFX311)',                                      
//             DSIN='W440.W440V4.W4405Y(+1)',                                   
//             PATHOUT='/app/vccs/qase/w440/data/w4405y.xls'                    
//*                                                                             
//*** old path PATHOUT='/volvo/vccsroot/w440/data/w4405y.xls'                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J05V                                         
